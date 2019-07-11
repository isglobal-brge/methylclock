#' Check wheter input data contains the required CpGs for the implemented clocks.
#' @param x data.frame (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet.
#'
#' @details To be supplied
#'
#' @export

checkClocks <- function(x, ...){
  if (inherits(x, "data.frame"))
    cpg.names <- rownames(x)
  else if (inherits(x, "ExpressionSet"))
    cpg.names <- Biobase::featureNames(x)
  else if (inherits(x, "GenomicRatioSet"))
    cpgs.names <- Biobase::featureNames(x)

  checkHorvat <- coefHorvath$CpGmarker[-1][!coefHorvath$CpGmarker[-1]%in%cpg.names]
  checkHannum <- coefHannum$Marker[-1][!coefHannum$Marker[-1]%in%cpg.names]
  checkGA <- coefGA$CpGmarker[-1][!coefGA$CpGmarker[-1]%in%cpg.names]
  checkLevine <- coefLevine$CpG[-1][!coefLevine$CpG[-1]%in%cpg.names]
  checkSkin <- coefSkin$CpG[-1][!coefSkin$CpG[-1]%in%cpg.names]

  sizes <- c(length(checkHorvat), length(checkHannum),
             length(checkGA), length(checkLevine),
             length(checkSkin))
  if (any(sizes!=0)){
    cat("There are some clock that cannot be computed since
        your data do not contain the required CpGs. These are: \n")
    out <- list(checkHorvat=checkHorvat, checkHannum=checkHannum,
                checkGA=checkGA, checkLevine=checkLevine,
                checkSkin=checkSkin)
  }
  else {
    cat("Your data contain the required CpGs for all clocks")
    out <- NULL
  }
  return(out)
}
