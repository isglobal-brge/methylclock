#' Check wheter input data contains the required 
#' CpGs for the implemented clocks.
#' @param    x data.frame or tibble (Individual in columns, 
#' CpGs in rows, CpG names in first colum - i.e. Horvath's format), 
#' ExpressionSet or GenomicRatioSet. 
#' A matrix is also possible having the CpG names in the rownames.
#' @param    ... other parameters
#' @details To be supplied
#' @examples
#' TestDataset <- get_TestDataset()
#' checkClocks(TestDataset)
#' @return a list with the different clocks when there are more than 80% of 
#' the required CpGs
#' @importFrom Biobase featureNames exprs
#' @export

checkClocks <- function(x, ...) {
    
    if (inherits(x, "data.frame") & !inherits(x, c("tbl", "tbl_df"))) {
        cpg.names <- x[, 1]
    } else if (inherits(x, "matrix")) {
        cpg.names <- rownames(x)
    } else if (inherits(x, c("tbl", "tbl_df"))) {
        cpg.names <- pull(MethylationData, 1)
    } else if (inherits(x, "ExpressionSet")) {
        cpg.names <- Biobase::featureNames(x)
    } else if (inherits(x, "GenomicRatioSet")) {
        cpgs.names <- Biobase::featureNames(x)
    }
    
    
    cpg.names <- getInputCpgNames(x)
    if( !all(c("coefHorvath", "coefHannum", "coefLevine", "coefSkin", 
            "coefPedBE", "coefWu", "coefTL", "coefBLUP", "coefEN") 
            %in%  ls(.GlobalEnv))) {
        load_DNAm_Clocks_data()
    }
    
    checkHorvath <- coefHorvath$CpGmarker[-1][!coefHorvath$CpGmarker[-1] 
                                              %in% cpg.names]
    checkHannum <- coefHannum$CpGmarker[!coefHannum$CpGmarker %in% cpg.names]
    checkLevine <- coefLevine$CpGmarker[-1][!coefLevine$CpGmarker[-1] 
                                            %in% cpg.names]
    checkSkin <- coefSkin$CpGmarker[-1][!coefSkin$CpGmarker[-1] %in% cpg.names]
    checkPedBE <- coefPedBE$CpGmarker[-1][!coefPedBE$CpGmarker[-1] 
                                          %in% cpg.names]
    checkWu <- coefWu$CpGmarker[-1][!coefWu$CpGmarker[-1] %in% cpg.names]
    checkTL <- coefTL$CpGmarker[-1][!coefTL$CpGmarker[-1] %in% cpg.names]
    checkBLUP <- coefBLUP$CpGmarker[-1][!coefBLUP$CpGmarker[-1] %in% cpg.names]
    checkEN <- coefEN$CpGmarker[-1][!coefEN$CpGmarker[-1] %in% cpg.names]
    

    sizes <- c(
        length(checkHorvath), length(checkHannum),
        length(checkLevine), length(checkSkin), length(checkPedBE),
        length(checkWu), length(checkTL), length(checkBLUP), length(checkEN)
    )
    n <- c(
        nrow(coefHorvath[-1, ]), nrow(coefHannum),
        nrow(coefLevine[-1, ]), nrow(coefSkin[-1, ]), nrow(coefPedBE[-1, ]),
        nrow(coefWu[-1, ]), nrow(coefTL[-1, ]), nrow(coefBLUP[-1, ]), 
        nrow(coefEN[-1, ])
    )
    df <- data.frame( clock = c( "Horvath", "Hannum", "Levine", "SkinHorvath",
                                "PedBE", "Wu", "TL", "BLUP", "EN" ), 
                      Cpgs_in_clock = n,
                      missing_CpGs=sizes, 
                      percentage = round( (sizes/n) * 100, 1))
    if (any(sizes != 0)) {
        message("There are some clocks that cannot be computed since your 
        data do not contain the required CpGs.These are the total number 
        of missing CpGs for each clock : \n \n")
        print(df)
        out <- list( Horvath = checkHorvath, Hannum = checkHannum,
                    Levine = checkLevine, skinHorvath = checkSkin,
                    PedBE = checkPedBE, Wu = checkWu, TL = checkTL,
                    BLUP = checkBLUP, EN = checkEN)
    }
    else {
        message("Your data contain the required CpGs for all clocks")
        out <- NULL
    }
    return(out)
}

