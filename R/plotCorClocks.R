#' Plot correlation among DNAm clockx
#' @param x a tible or data.frame with the different DNAm clocks 
#' @param ... other arguments to be passs through function 'chart.Correlation' from 'PerformanceAnalytics' package
#'               
#'
#' @details To be supplied
#'
#' @export

plotCorClocks <- function(x, ...) {
  clocks <- c("age", "Horvath", "Levine", "BNN", "Horvath2",
            "Knigth", "Bohlin", "Mayne", "Lee")
  sel <- intersect(clocks, colnames(x))
  x.sel <- x[, sel]
  no.na <- apply(x.sel, 2, function(x) !all(is.na(x)))
  x.nona <- x.sel[, no.na]
  PerformanceAnalytics::chart.Correlation(x.nona, histogram=TRUE, pch=19, ...)
}
