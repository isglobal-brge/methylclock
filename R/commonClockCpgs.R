#' Get common CpGs
#'
#' Show the required CpGs contained on input data for the implemented clocks
#'
#' @param object resulting object from checkClocks functions
#' @param clock string with the implemented clock, possible values are :
#' "Knight", "Bohlin", "Mayne" and "Lee", "Horvath", "Hannum", "Levine",
#' "skinHorvath", "PedBE", "Wu" and "TL"
#' @examples
#' TestDataset <- get_TestDataset()
#' cpgs.missing.GA <- checkClocksGA(TestDataset)
#' cpgs.missing <- checkClocks(TestDataset)
#' commonClockCpgs(cpgs.missing.GA, "Bohlin")
#' commonClockCpgs(cpgs.missing, "Hannum")
#' @return The common CpGs between input data and defined GA clock
#'
#' @export

commonClockCpgs <- function(object, clock) {
    available.clocks <- c( "Knight", "Bohlin", "Mayne", "Lee",
                            "Horvath", "Hannum", "Levine", "BNN",
                            "skinHorvath", "PedBE", "Wu", "TL" )
    
    if (sum(available.clocks %in% names(object)) == 0) {
        stop("Object don't contain any GA clock information")
    }
    if (!clock %in% available.clocks) {
        stop("You wrote the name of an unavailable clock.
            Available clocks are: Knight, Bohlin, Mayne and Lee")
    }
    
    res <- switch(clock, "Knight" = object$Knight, "Bohlin" = object$Bohlin,
                    "Mayne" = object$Mayne, "Lee" = object$Lee,
                    "Horvath" = object$Horvath, "Hannum" = object$Hannum,
                    "Levine" = object$Levine, "BNN" = object$Horvath,
                    "skinHorvath" = object$SkinHorvath, "PedBE" = object$PedBE,
                    "Wu" = object$Wu, "TL" = object$TL )
    return(res)
}
