#' Check wheter input data contains the required CpGs for the implemented clocks for Gestational Age.
#' @param x data.frame or tibble (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet. A matrix is also possible having the CpG names in the rownames.
#'
#' @details To be supplied
#'
#' @export

checkClocksGA <- function(x,  ...){
  if (inherits(x, "data.frame") & !inherits(x, c("tbl", "tbl_df")))
    cpg.names <- x[,1]
  else if (inherits(x, "matrix"))
    cpg.names <- rownames(x)
  else if (inherits(x, c("tbl", "tbl_df")))
    cpg.names <- pull(MethylationData,1)
  else if (inherits(x, "ExpressionSet"))
    cpg.names <- Biobase::featureNames(x)
  else if (inherits(x, "GenomicRatioSet"))
    cpgs.names <- Biobase::featureNames(x)

  checkKnigth <- coefKnigthGA$CpGmarker[-1][!coefKnigthGA$CpGmarker[-1]%in%cpg.names]
  checkBohlin <- coefBohlinGA$cpgs[!coefBohlinGA$cpgs%in%cpg.names]
  checkMayne <- coefMayneGA$cpg[-1][!coefMayneGA$cpg[-1]%in%cpg.names]
  checkLee <- coefLeeGA$CpGs[-1][!coefLeeGA$CpGs[-1]%in%cpg.names]
  
  
  sizes <- c(length(checkKnigth), length(checkBohlin),
             length(checkMayne), length(checkLee))
  names(sizes) <- c("Knigth", "Bohlin", "Mayne", "Lee")
  if (any(sizes!=0)){
    cat("There are some clocks that cannot be computed since your data do not contain the required CpGs. 
        These are the total number of missing CpGs for each clock : \n \n")
    print(sizes)
    
    out <- list(checkKnigth=checkKnigth, checkBohlin=checkBohlin,
                checkMayne=checkMayne, checkLee=checkLee)
  }
  else {
    cat("Your data contain the required CpGs for all clocks")
    out <- NULL
  }
  return(out)
}
