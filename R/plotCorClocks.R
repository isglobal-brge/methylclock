#' Plot correlation among DNAm clockx
#' @param x a tible or data.frame with the different DNAm clocks
#' @param ... other arguments to be passs through function 
#' 'chart.Correlation' from 'PerformanceAnalytics' package
#'
#' @details To be supplied
#'
#' @return Plot with Correlation Clocks
#'
#' @examples
#' \donttest{
#' library(Biobase)
#' library(GEOquery)
#'
#' dd <- GEOquery::getGEO("GSE109446")
#' gse109446 <- dd[[1]]
#' controls <- Biobase::pData(gse109446)$`diagnosis:ch1` == "control"
#' gse <- gse109446[, controls]
#' age <- as.numeric(Biobase::pData(gse)$`age:ch1`)
#' age.gse <- DNAmAge(gse, age = age)
#' plotCorClocks(age.gse)
#' }
#'
#' @import ggplot2 ggpubr PerformanceAnalytics
#'
#' @export

plotCorClocks <- function(x, ...) {
    clocks <- c( "Horvath", "Levine", "BNN", "skinHorvath", "Hannum", "PedBE",
                "skinHorvath", "Knigth", "Bohlin", "Mayne", "Lee" )
    sel <- intersect(clocks, colnames(x))
    x.sel <- x[, sel]
    no.na <- apply(x.sel, 2, function(x) !all(is.na(x)))
    x.nona <- x.sel[, no.na]
    nclocks <- ncol(x.nona)
    ee <- tidyr::gather(x.nona, key = method, value = clock) %>%
        add_column(age = rep(x$age, nclocks))

    ggplot(ee, aes(x = clock, y = age)) +
        geom_point() +
        geom_smooth(method = lm, se = FALSE) +
        xlab("DNAm clock") +
        ylab("Chronological Age") +
        ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label..,
            sep = "~`,`~" ))) +
        facet_grid(~method)
}
