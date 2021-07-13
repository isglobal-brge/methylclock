#' Plot DNAm age estimation vs chronological age.
#' @param x DNAm age estimation
#' @param y Chronological age
#' @param clock Type of clock 'chronological' or 'GA',
#' default 'chronological'
#' @param tit Plot title. Default is "Horvath's method".
#' @param ... Other plot parameters for ggplot
#'
#' @return Plot with estimated DNAmAge
#'
#' @examples
#'
#' library(tidyverse)
#'
#' path <- system.file("extdata", package = "methylclock")
#' covariates <- read_csv(file.path(
#'   path,
#'   "SampleAnnotationExample55.csv"
#' ))
#' age <- covariates$Age
#' MethylationData <- get_MethylationDataExample()
#'
#' age.example55 <- DNAmAge(MethylationData)
#' plotDNAmAge(age.example55$Horvath, age)
#' @import ggplot2
#' @importFrom ggpmisc stat_poly_eq
#' @export

plotDNAmAge <- function(x, y, tit = "Horvath's method",
                        clock = "chronological", ...) {
    df <- data.frame(x = x, y = y)
    my.formula <- y ~ x
    if (missing(clock)) {
        yy <- "Chronological Age"
    } else if (clock == "GA") {
        yy <- "Gestational Age"
    }
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
        ggplot2::geom_smooth( method = "lm", se = FALSE, color = "black",
                                formula = my.formula ) +
        ggplot2::xlab("DNA Methylation Age") +
        ggplot2::ylab(yy) +
        ggplot2::ggtitle(tit) +
        ggpmisc::stat_poly_eq( formula = my.formula,
                                ggplot2::aes(label = paste(..eq.label..,
                                                            ..rr.label..,
                                                            sep = "~~~")),
                                parse = TRUE) +
        ggplot2::geom_point()
    p
}
