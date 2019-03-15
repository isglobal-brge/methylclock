#' DNAm age estimation using Horvath's, Hannum's and Levine's method.
#' @param x matrix ... ponemos un MethlylSet? rows -> CpGs columns 1ª nombres
#' @param GestationalAge Is gestational age clock estimated? Default is FALSE.
#' @param toBetas Should data be transformed to beta values? Default is FALSE. If TRUE, it implies data are M values.
#' @param fastImp Is fast imputation performed if necessary? (see details). Default is FALSE
#' @param normalize Is Horvath's normalization performed? By default is FALSE
#' @param cell.count Are IEAA and EEAA computed? Default is TRUE.
#' @param cell.count.reference Used when 'cell.count' is TRUE. Default is "blood gse35069 complete". See 'meffil::meffil.list.cell.count.references()' for possible values.
#' @param age individual's chronological age. Required when 'cell.count' is TRUE.
#'
#' @details Imputation is performed when having missing data.
#'          Fast imputation is performed by ...
#'          what about imputing only when CpGs for the clock are missing?
#'
#' @export


DNAmAge <- function(x, GestationalAge=FALSE,
                    toBetas=FALSE,
                    fastImp=FALSE,
                    normalize=FALSE,
                    cell.count=TRUE,
                    cell.count.reference = "blood gse35069 complete",
                    age){

  if (inherits(x, "data.frame")){
    cpgs <- t(as.matrix(x[, -1]))
    colnames(cpgs) <- as.character(x[, 1, drop=TRUE])
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

  if (GestationalAge) {
    coefHorvath <- coefGA
  }

  CpGs.model <- as.character(coefHorvath$CpGmarker[-1])

  if(any(!CpGs.model%in%colnames(cpgs))){
    stop("CpGs in Horvat's model are not present in your data.")
  }

  miss <- apply(cpgs[, CpGs.model], 1, function(x) any(is.na(x)))

  if (any(miss)){
    warning(paste("There are missing values. Those have been imputed. \n"))
    if (fastImp){
      cpgs.imp <- cpgs
      for (i in which(miss)) {
        sel <- is.na(cpgs[i, ])
        cpgs.imp[i, sel] <- probeAnnotation21kdatMethUsed$goldstandard2[sel]
      }
    }
    else{
      cpgs.imp <- t(impute.knn(t(cpgs))$data)
    }
  }
  else{
    cpgs.imp <- cpgs
  }

  if (normalize) {
    cpgs.norm <- BMIQcalibration(
      datM = cpgs.imp,
      goldstandard.beta = probeAnnotation21kdatMethUsed$goldstandard2,
      plots = FALSE
    )
  }

  else {
    cpgs.norm <- cpgs.imp
  }

  selectCpGsClock <- is.element(colnames(cpgs.norm),
                               CpGs.model)
  if (sum(selectCpGsClock) < nrow(coefHorvath) - 1) {
    stop(
      "The CpGs names needed to calculate DNAm age method are not in your input data.
  (e.g. column 1 of your data.frame or rownames in your ExpressionSet).
  Check whether they are cg numbers such as cg00075967."
    )
  }
  if (sum(selectCpGsClock) > nrow(coefHorvath) - 1) {
    stop(
      "There are duplicated CpGs. Each row should report only one unique
       CpG marker (e.g. cg number)."
    )
  }

  cpgs.norm.s <- cpgs.norm[, coefHorvath$CpGmarker[-1]]

  if(GestationalAge){
    DNAmAge <- coefHorvath$CoefficientTraining[1] +
      cpgs.norm.s%*%coefHorvath$CoefficientTraining[-1]
    if (!missing(age))
      out <- data.frame(id = rownames(DNAmAge),
                      GA = DNAmAge,
                      GAacc = DNAmAge - age,
                      age = age)
    else
      out <- data.frame(id = rownames(DNAmAge),
                        GA = DNAmAge)

    out <- tibble::as_tibble(out)
  }

  else{
    DNAmAge <- anti.trafo(coefHorvath$CoefficientTraining[1] +
                            cpgs.norm.s%*%coefHorvath$CoefficientTraining[-1])

  if(cell.count){
    if (missing(age)){
      stop("Chronological age is required. Pass it through the argument 'age'")
    }
    cell.counts <- meffil::meffil.estimate.cell.counts.from.betas(t(cpgs),
                                                         cell.count.reference)
    df <- data.frame(DNAmAge = DNAmAge,
                     age = age,
                     cell.counts)
    ok <- which(apply(cell.counts, 2, IQR) > 10e-6)
    df <- df[,ok]

    mod.ieaa <- lm(DNAmAge ~ age, data=df,
                    na.action="na.exclude")
    mod.eeaa <- lm(DNAmAge ~ ., data=df,
                    na.action="na.exclude")
    IEAA <- resid(mod.ieaa)
    EEAA <- resid(mod.eeaa)
    horvath <- data.frame(Horvath = DNAmAge[,1],
                          AgeAcDiff = DNAmAge[,1] - age,
                          IEAA = IEAA,
                          EEAA = EEAA)
  }
  else{
    horvath <- data.frame(Horvath = DNAmAge[,1])
  }


  if (sum(is.element(colnames(cpgs.norm), coefLevine$CpG)) !=
                (nrow(coefLevine) -1)) {
    warning(
      "The CpGs needed for Levine's method are not in your input data. This method will not be computed"
    )
    cpgs.levine <- rep(NA, nrow(cpgs.norm))
  }
  else {
    cpgs.levine <- cpgs.norm[, coefLevine$CpG[-1]]
    levine <- coefLevine$Weight[1] +
      cpgs.levine%*%coefLevine$Weight[-1]
  }

  if (sum(is.element(colnames(cpgs.norm), coefHannum$Marker)) !=
      nrow(coefHannum)) {
    warning("The CpGs needed for Hannum's method are not in your input data. This method will not be computed")
    hannum <- data.frame(Hannum = rep(NA, nrow(cpgs.norm)))
  }
  else{
    cpgs.hannum <- cpgs.norm[, coefHannum$Marker]
    predAge <- cpgs.hannum%*%coefHannum$Coefficient
    hannum <- data.frame(Hannum = predAge)
    if (!missing(age)){
      hannum <- data.frame(Hannum = predAge,
                               AMAR = predAge/age)
    }
  }

  cpgs.bn <- t(cpgs.norm[,coefHorvath$CpGmarker[-1]])
  bn <- main_NewModel1Clean(cpgs.bn)

  if (!missing(age))
    out <- data.frame(id = rownames(DNAmAge),
                      horvath,
                      hannum,
                      Levine = levine,
                      BNN = bn,
                      age = age)
  else
    out <- data.frame(id = rownames(DNAmAge),
                      horvath,
                      hannum,
                      Levine = levine,
                      BayNet = bn)

  out <- tibble::as_tibble(out)
  attr(out, "cell_proportion") <- cell.counts
  }

  out
}
