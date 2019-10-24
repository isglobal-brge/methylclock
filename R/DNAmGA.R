#' Gestational DNAm age estimation using different DNA methylation clocks.
#' @param x data.frame (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet.
#' @param toBetas Should data be transformed to beta values? Default is FALSE. If TRUE, it implies data are M values.
#' @param fastImp Is fast imputation performed if necessary? (see details). Default is FALSE
#' @param normalize Is Horvath's normalization performed? By default is FALSE
#' @param age individual's chronological age. Required to compute gestational age difference output
#' @param cell.count Are cell counts estimated? Default is TRUE.
#' @param cell.count.reference Used when 'cell.count' is TRUE. Default is "blood gse35069 complete". See 'meffil::meffil.list.cell.count.references()' for possible values.
#'
#' @details Imputation is performed when having missing data.
#'          Fast imputation is performed by ...
#'          what about imputing only when CpGs for the clock are missing?
#'
#' @export


DNAmGA <- function(x, toBetas=FALSE,
                   fastImp=FALSE,
                   normalize=FALSE,
                   age, 
                   cell.count=TRUE, 
                   cell.count.reference = "andrews and bakulski cord blood",
                   ...){
  
  if (inherits(x, "data.frame")){
    cpgs.names <- as.character(x[, 1, drop=TRUE]) 
    if (length(grep("cg", cpgs.names))==0)
      stop("First column should contain CpG names")
    cpgs <- t(as.matrix(x[, -1]))
    colnames(cpgs) <- cpgs.names
  }
  else if (inherits(x, "ExpressionSet")){
    cpgs <- t(Biobase::exprs(x))
  }
  else if (inherits(x, "GenomicRatioSet")){
    cpgs <- t(minfi::getBeta(x))
  }
  else {
    stop("x must be a data.frame or a 'GenomicRatioSet' or a 'ExpressionSet' object")
  }
  
  if (toBetas){
    toBeta <- function (m) {
      2^m/(2^m + 1)
    }
    cpgs <- toBeta(cpgs)
  }
  
  if(any(cpgs < -0.1 | cpgs >1.1, na.rm=TRUE))
    stop("Data seems to do not be beta values. Check your data or set 'toBetas=TRUE'")
  
  cpgs.all <- c(as.character(coefKnigthGA$CpGmarker[-1]),
                GAprediction::extractSites(),
                as.character(coefMayneGA$CpGmarker),
                as.character(coefLeeGA$CpGmarker[-1]))
  
  if(any(!cpgs.all%in%colnames(cpgs))){
    warning("CpGs in all Gestational Age clocks are not present in your data. Try 'checkClocksGA' function
         to find the missing CpGs of each method.")
  }
  
  cpgs.in <- intersect(cpgs.all, colnames(cpgs))
  
  miss <- apply(cpgs[, cpgs.in], 2, function(x) any(is.na(x)))
  
  if (any(miss)){
    if (fastImp){
      cat(paste("Imputing missing data of", sum(miss), "CpGs .... \n"))
      mm <- apply(cpgs, 2, median, na.rm=TRUE)
      cpgs.imp <- sweep(cpgs, 2, STATS=mm, 
                        FUN = function(x,s) ifelse(is.na(x), s, x))
    }
    else{
      quiet <- function(x) { 
        sink(tempfile()) 
        on.exit(sink()) 
        invisible(force(x)) 
      } 
      cat(paste("Imputing missing data of the entire matrix .... \n"))
      cpgs.imp <- quiet(t(impute.knn(t(cpgs), ...)$data))
    }
    cat("Data imputed. Starting DNAm clock estimation ... \n")
  }
  else{
    cpgs.imp <- cpgs
  }
  
  

# --------------> Knigth
  
    knigth <- predAge(cpgs.imp, coefKnigthGA, intercept = TRUE)
    Knigth <- data.frame(id = rownames(cpgs.imp),
                           Knigth = knigth)
  
# --------------> Bohlin
  
   bohlin <- try(GAprediction::predictGA(cpgs.imp)[,1]/52, TRUE)
   if (inherits(bohlin, "try-error"))
     bohlin <- rep(NA, nrow(cpgs.imp))
   Bohlin <- data.frame(id = rownames(cpgs.imp),
                         Bohlin = bohlin)
  
# --------------> Mayne
  
    mayne <- predAge(cpgs.imp, coefMayneGA, intercept = TRUE)
    Mayne <- data.frame(id = rownames(cpgs.imp),
                          Mayne = mayne)
      
# --------------> Lee
  
  
  if(mean(coefLeeGA$CpGmarker[-1]%in%colnames(cpgs.imp))>0.8) {
    cpgs.imp.s <- cpgs.imp[, coefLeeGA$CpGs[-1]]
    Lee.RPC <- coefLeeGA$Coefficient_RPC[1] +
      cpgs.imp.s%*%coefLeeGA$Coefficient_RPC[-1]
    Lee.CPC <- coefLeeGA$Coefficient_CPC[1] +
      cpgs.imp.s%*%coefLeeGA$Coefficient_CPC[-1]
    Lee.refRPC <- coefLeeGA$Coefficient_refined_RPC[1] +
      cpgs.imp.s%*%coefLeeGA$Coefficient_refined_RPC[-1]
    if (!missing(age))
      Lee <- data.frame(id = rownames(cpgs.imp),
                      Lee.RPC = Lee.RPC,
                      Lee.CPC = Lee.CPC,
                      Lee.refRPC = Lee.refRPC,
                      Lee.RPCdiff = Lee.RPC - age,
                      Lee.CPCdiff = Lee.CPC - age,
                      Lee.refRPCdiff = Lee.refRPC - age,
                      age = age)
    else
      Lee <- data.frame(id = rownames(cpgs.imp),
                      Lee.RPC = Lee.RPC,
                      Lee.CPC = Lee.CPC,
                      Lee.refRPC = Lee.refRPC)
  }
  else{
    warning("The number of missing CpGs for Lee clocks exceeds 80%.\n  ---> This DNAm clock will be NA.")
  
      Lee <- data.frame(id = rownames(cpgs.imp),
                      Lee = rep(NA, nrow(cpgs.imp)))
  }
  
# --------------> output
    
    if (!missing(age)){
      if (!cell.count) {
        Knigth <- ageAcc1(Knigth, age, lab="Knigth")
        Bohlin <- ageAcc1(Bohlin, age, lab="Bohlin")
        Mayne <- ageAcc1(Mayne, age, lab="Mayne")
      }
      else {
        cell.counts <- try(meffil::meffil.estimate.cell.counts.from.betas(t(cpgs),
                                                                      cell.count.reference), TRUE)
        if (inherits(cell.counts, "try-error"))
          stop("cell counts cannot be estimated since your data have missing CpGs for meffil")
        else {
          ok <- which(apply(cell.counts, 2, IQR) > 10e-6)
          cell.counts <- cell.counts[,ok]
          df <- data.frame(age=age, cell.counts)
        
          Knigth <- ageAcc2(Knigth, df, lab="Knigth")
          Bohlin <- ageAcc2(Bohlin, df, lab="Bohlin")
          Mayne <- ageAcc2(Mayne, df, lab="Mayne")
        }
      }
    }
    else {
      cell.count <- FALSE
    }
    
  out <- Knigth %>% full_join(Bohlin, by="id") %>% 
    full_join(Mayne, by="id") %>% full_join(Lee, by="id")
  out <- tibble::as_tibble(out)
  
  if (!missing(age))
      out <- add_column(out, age=age)
  
  if (cell.count)
    attr(out, "cell_proportion") <- cell.counts
  
  out
}
