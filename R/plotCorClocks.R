#' Plot correlation among DNAm clockx
#' @param x a tible or data.frame with the different DNAm clocks 
#' @param ... other arguments to be passs through function 'chart.Correlation' from 'PerformanceAnalytics' package
#'               
#'
#' @details To be supplied
#'
#' @export

plotCorClocks <- function(x, ...) {
  clocks <- c("Horvath", "Levine", "BNN", "Horvath2", "Hannum", "PedBE", "skinHorvath",
            "Knigth", "Bohlin", "Mayne", "Lee")
  sel <- intersect(clocks, colnames(x))
  x.sel <- x[, sel]
  no.na <- apply(x.sel, 2, function(x) !all(is.na(x)))
  x.nona <- x.sel[, no.na]
  nclocks <- ncol(x.nona)
  ee <- gather(x.nona, key=method, value=clock) %>% add_column(age=rep(x$age, nclocks))
  
  ggplot(ee, aes(x=clock, y=age)) + geom_point() + geom_smooth(method=lm, se=FALSE) + xlab("DNAm clock") + ylab("Chronological Age") + 
    ggpubr::stat_cor() + facet_grid(~method)
  
  
}
