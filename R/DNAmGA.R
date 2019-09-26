#' Gestational DNAm age estimation using different DNA methylation clocks.
#' @param x data.frame (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet.
#' @param toBetas Should data be transformed to beta values? Default is FALSE. If TRUE, it implies data are M values.
#' @param fastImp Is fast imputation performed if necessary? (see details). Default is FALSE
#' @param normalize Is Horvath's normalization performed? By default is FALSE
#' @param age individual's chronological age. Required to compute gestational age difference output
#'
#' @details Imputation is performed when having missing data.
#'          Fast imputation is performed by ...
#'          what about imputing only when CpGs for the clock are missing?
#'
#' @export


DNAmGA <- function(x, toBetas=FALSE,
                   fastImp=FALSE,
                   normalize=FALSE,
                   age, ...){
  
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
                  as.character(coefBohlinGA$cpgs),
                as.character(coefMayneGA$cpg),
                  as.character(coefLeeGA$CpGs[-1]))
  
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
  
  if (normalize) {
    cpgs.norm <- BMIQcalibration(
      datM = cpgs.imp,
      goldstandard.beta = probeAnnotation21kdatMethUsed$goldstandard2,
      plots = FALSE)
  }
  
  else {
    cpgs.norm <- cpgs.imp
  }
  

# --------------> Knigth
  
  if(all(coefKnigthGA$CpGmarker[-1]%in%colnames(cpgs))) {
    cpgs.norm.s <- cpgs.norm[, coefKnigthGA$CpGmarker[-1]]
    Knigth <- coefKnigthGA$CoefficientTraining[1] +
      cpgs.norm.s%*%coefKnigthGA$CoefficientTraining[-1]
    if (!missing(age))
      Knigth <- data.frame(id = rownames(Knigth),
                           Knigth = Knigth,
                           KnigthDiff = Knigth - age,
                           age = age)
    else
      Knigth <- data.frame(id = rownames(Knigth),
                           Knigth = Knigth)
  }
  else{
    Knigth <- data.frame(id = rownames(cpgs.norm),
                         Knigth = rep(NA, nrow(cpgs.norm)))
  }
  

# --------------> Bohlin
  
  if(all(coefBohlinGA$cpgs%in%colnames(cpgs))) {
    cpgs.norm.s <- cpgs.norm[, coefBohlinGA$cpgs]
    Bohlin <- cpgs.norm.s%*%coefBohlinGA$coef
    if (!missing(age))
      Bohlin <- data.frame(id = rownames(Bohlin),
                         Bohlin = Bohlin,
                         BohlinDiff = Bohlin - age,
                         age = age)
    else
      Bohlin <- data.frame(id = rownames(Bohlin),
                         Bohlin = Bohlin)
  }
  else{
    Bohlin <- data.frame(id = rownames(cpgs.norm),
                         Bohlin = rep(NA, nrow(cpgs.norm)))
  }

  
# --------------> Mayne
  
  if(all(coefMayneGA$cpg[-1]%in%colnames(cpgs))) {
    cpgs.norm.s <- cpgs.norm[, coefMayneGA$cpg[-1]]
    Mayne <- coefMayneGA$Coefficient[1] + cpgs.norm.s%*%coefMayneGA$Coefficient[-1]
    if (!missing(age))
      Mayne <- data.frame(id = rownames(Mayne),
                          Mayne = Mayne,
                          MayneDiff = Mayne - age,
                           age = age)
    else
      Mayne <- data.frame(id = rownames(Mayne),
                          Mayne = Mayne)
  }
  else{
    Mayne <- data.frame(id = rownames(cpgs.norm),
                        Mayne = rep(NA, nrow(cpgs.norm)))
  }
      
# --------------> Lee
  
  
  if(all(coefLeeGA$CpGs[-1]%in%colnames(cpgs))) {
    cpgs.norm.s <- cpgs.norm[, coefLeeGA$CpGs[-1]]
    Lee.RPC <- coefLeeGA$Coefficient_RPC[1] +
      cpgs.norm.s%*%coefLeeGA$Coefficient_RPC[-1]
    Lee.CPC <- coefLeeGA$Coefficient_CPC[1] +
      cpgs.norm.s%*%coefLeeGA$Coefficient_CPC[-1]
    Lee.refRPC <- coefLeeGA$Coefficient_refined_RPC[1] +
      cpgs.norm.s%*%coefLeeGA$Coefficient_refined_RPC[-1]
    if (!missing(age))
      Lee <- data.frame(id = rownames(Lee.RPC),
                      Lee.RPC = Lee.RPC,
                      Lee.CPC = Lee.CPC,
                      Lee.refRPC = Lee.refRPC,
                      Lee.RPCdiff = Lee.RPC - age,
                      Lee.CPCdiff = Lee.CPC - age,
                      Lee.refRPCdiff = Lee.refRPC - age,
                      age = age)
    else
      Lee <- data.frame(id = rownames(Lee.RPC),
                      Lee.RPC = Lee.RPC,
                      Lee.CPC = Lee.CPC,
                      Lee.refRPC = Lee.refRPC)
  }
  else{
    Lee <- data.frame(id = rownames(cpgs.norm),
                      Lee = rep(NA, nrow(cpgs.norm)))
  }
  

# --------------> output
  
  out <- Knigth %>% full_join(Bohlin, by="id") %>% full_join(Mayne, by="id") %>% full_join(Lee, by="id")
  out <- tibble::as_tibble(out)
  out
}
