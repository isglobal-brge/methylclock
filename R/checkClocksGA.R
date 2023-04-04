#' Check wheter input data contains the required CpGs for the implemented clocks for Gestational Age.
#' @param x data.frame or tibble (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), ExpressionSet or GenomicRatioSet. A matrix is also possible having the CpG names in the rownames.
#' @param ... other parameters
#'
#' @details To be supplied
#'
#' @examples
#' checkClocksGA(TestDataset)
#' @importFrom Biobase featureNames exprs
#' @return a list with the different GA clocks when there are more than 80% of the required CpGs
#' @export

checkClocksGA <- function(x, ...) {
    # if (inherits(x, "data.frame") & !inherits(x, c("tbl", "tbl_df"))) {
    #   cpg.names <- x[, 1]
    # } else if (inherits(x, "matrix")) {
    #   cpg.names <- rownames(x)
    # } else if (inherits(x, c("tbl", "tbl_df"))) {
    #   cpg.names <- pull(x, 1)
    # } else if (inherits(x, "ExpressionSet")) {
    #   cpg.names <- Biobase::featureNames(x)
    # } else if (inherits(x, "GenomicRatioSet")) {
    #   cpgs.names <- Biobase::featureNames(x)
    # }
  
    cpg.names <- getCpGsFeatures(x)

    checkKnight <- coefKnightGA$CpGmarker[-1][!coefKnightGA$CpGmarker[-1] %in% cpg.names]
    checkBohlin <- coefBohlin$CpGmarker[-1][!coefBohlin$CpGmarker[-1] %in% cpg.names]
    # checkBohlin <- coefBoh[!coefBoh %in% cpg.names]
    checkMayne <- coefMayneGA$CpGmarker[-1][!coefMayneGA$CpGmarker[-1] %in% cpg.names]
    checkLee <- coefLeeGA$CpGmarker[-1][!coefLeeGA$CpGmarker[-1] %in% cpg.names]
    checkEPIC <- coefEPIC$CpGmarker[-1][!coefEPIC$CpGmarker[-1] %in% cpg.names]
    
    
    sizes <- c(
        length(checkKnight), length(checkBohlin),
        length(checkMayne), length(checkLee),
        length(checkEPIC)
    )
    
    n <- c(
        nrow(coefKnightGA) - 1, nrow(coefBohlin) - 1,
        nrow(coefMayneGA) - 1, nrow(coefLeeGA) - 1, 
        nrow(coefEPIC) - 1
    )
    
    df <- data.frame(
        clock = c("Knight", "Bohlin", "Mayne", "Lee", "EPIC"),
        Cpgs_in_clock = n,
        missing_CpGs = sizes,
        percentage = round((sizes / n) * 100, 1)
    )
    
    if (any(sizes != 0)) {
        cat("There are some clocks that cannot be computed since your data do not contain the required CpGs. 
            These are the total number of missing CpGs for each clock : \n \n")
        print(df)
        
        out <- list(
          Knight = checkKnight, Bohlin = checkBohlin,
          Mayne = checkMayne, Lee = checkLee,  EPICGA = checkEPIC
        )
    }
    else {
        cat("Your data contain the required CpGs for all clocks")
        out <- NULL
    }
        return(invisible(out))
    }
