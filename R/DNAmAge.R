#' DNAm age estimation using different DNA methylation clocks.
#' @param x data.frame (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet.
#' @param toBetas Should data be transformed to beta values? Default is FALSE. If TRUE, it implies data are M values.
#' @param fastImp Is fast imputation performed if necessary? (see details). Default is FALSE
#' @param normalize Is Horvath's normalization performed? By default is FALSE
#' @param age individual's chronological age. 
#' @param cell.count Are cell counts estimated? Default is TRUE.
#' @param cell.count.reference Used when 'cell.count' is TRUE. Default is "blood gse35069 complete". See 'meffil::meffil.list.cell.count.references()' for possible values.
#'
#' @details Imputation is performed when having missing data.
#'          Fast imputation is performed by ...
#'          what about imputing only when CpGs for the clock are missing?
#'
#' @export


DNAmAge <- function(x, 
                    toBetas=FALSE,
                    fastImp=FALSE,
                    normalize=FALSE,
                    age,
                    cell.count=TRUE,
                    cell.count.reference = "blood gse35069 complete",
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
  

  cpgs.all <- c(coefHorvath$CpGmarker,
                coefHannum$CpGmarker,
                coefLevine$CpGmarker, 
                coefSkin$CpGmarker)
  cpgs.in <- intersect(cpgs.all, colnames(cpgs))
  
  miss <- apply(cpgs[, cpgs.in], 2, function(x) any(is.na(x)))
  
  if (any(miss)){
    if (fastImp){
      cpgs <- cpgs[,cpgs.in]
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
  
  DNAmAge <- predAge(cpgs.imp, coefHorvath, intercept=TRUE)
  horvath <- anti.trafo(DNAmAge)
  Horvath <- data.frame(id = rownames(cpgs.imp),
                        Horvath = horvath)
  
  
  levine <- predAge(cpgs.imp, coefLevine, intercept=TRUE)
  Levine <- data.frame(id = rownames(cpgs.imp),
                        Levine = levine)
  
   
  hannum <- predAge(cpgs.imp, coefHannum, intercept=FALSE)
  Hannum <- data.frame(id = rownames(cpgs.imp),
                        Hannum = hannum)
  
  
  skinHorvath <- predAge(cpgs.imp, coefSkin, intercept=TRUE)
  skinHorvath <- anti.trafo(skinHorvath)
  skinHorvath <- data.frame(id = rownames(cpgs.imp),
                        skinHorvath = skinHorvath)
  
    
  if(any(!coefHorvath$CpGmarker[-1]%in%colnames(cpgs.imp))){
   warning("Bayesian method cannot be estimated")
   bn <- rep(NA, nrow(cpgs.imp))
  }
  else {
   cpgs.bn <- t(cpgs.imp[,coefHorvath$CpGmarker[-1]])
   bn <- main_NewModel1Clean(cpgs.bn)
  }
  
  BNN <- data.frame(id = rownames(cpgs.imp),
                            BNN = bn)
  
  if (!missing(age)){
    if (!cell.count) {
      Horvath <- ageAcc1(Horvath, age, lab="Horvath")
      Hannum <- ageAcc1(Hannum, age, lab="Hannum")
      BNN <- ageAcc1(BNN, age, lab="BNN")
      Levine <- ageAcc1(Levine, age, lab="Levine")
      skinHorvath <- ageAcc1(skinHorvath, age, lab="skin")
    }
    else {
      cell.counts <- meffil::meffil.estimate.cell.counts.from.betas(t(cpgs),
                                                                    cell.count.reference)
      ok <- which(apply(cell.counts, 2, IQR) > 10e-6)
      cell.counts <- cell.counts[,ok]
      df <- data.frame(age=age, cell.counts)
      
      Horvath <- ageAcc2(Horvath, df, lab="Horvath")
      Hannum <- ageAcc2(Hannum, df, lab="Hannum")
      BNN <- ageAcc2(BNN, df, lab="BNN")
      Levine <- ageAcc2(Levine, df, lab="Levine")
      skinHorvath <- ageAcc2(skinHorvath, df, lab="Hovarth2")
    }
  }
  else {
    cell.count <- FALSE
  }
  
  out <- Horvath %>% 
    full_join(Hannum, by="id") %>% 
    full_join(Levine, by="id") %>%
    full_join(BNN, by="id") %>% 
    full_join(skinHorvath, by="id")   
  out <- tibble::as_tibble(out)
  
  if (!missing(age))
    out <- add_column(out, age=age)
  
  if (cell.count)
   attr(out, "cell_proportion") <- cell.counts
  
  out
}
