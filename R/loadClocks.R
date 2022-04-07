# Load DNAm clock data

#' Loads DNAm clock data from methylclockData
#' @examples
#' load_DNAm_Clocks_data()
#' @return void
#' @export
load_DNAm_Clocks_data <- function() {
    if (!"coefHorvath" %in% ls()) {
        coefHorvath <- get_coefHorvath()
        assign("coefHorvath", coefHorvath, envir = .GlobalEnv)
    }
    if (!"coefHannum" %in% ls()) {
        coefHannum <- get_coefHannum()
        assign("coefHannum", coefHannum, envir = .GlobalEnv)
    }
    if (!"coefLevine" %in% ls()) {
        coefLevine <- get_coefLevine()
        assign("coefLevine", coefLevine, envir = .GlobalEnv)
    }
    if (!"coefSkin" %in% ls()) {
        coefSkin <- get_coefSkin()
        assign("coefSkin", coefSkin, envir = .GlobalEnv)
    }
    if (!"coefPedBE" %in% ls()) {
        coefPedBE <- get_coefPedBE()
        assign("coefPedBE", coefPedBE, envir = .GlobalEnv)
    }
    if (!"coefWu" %in% ls()) {
        coefWu <- get_coefWu()
        assign("coefWu", coefWu, envir = .GlobalEnv)
    }
    if (!"coefTL" %in% ls()) {
        coefTL <- get_coefTL()
        assign("coefTL", coefTL, envir = .GlobalEnv)
    }
    if (!"coefEN" %in% ls()) {
        coefEN <- get_coefEN()
        assign("coefEN", coefEN, envir = .GlobalEnv)
    }
    if (!"coefBLUP" %in% ls()) {
        coefBLUP <- get_coefBLUP()
        assign("coefBLUP", coefBLUP, envir = .GlobalEnv)
    }
}
