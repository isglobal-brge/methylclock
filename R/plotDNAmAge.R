#' Plot DNAm age estimation vs chronological age.
#' @param x DNAm age estimation
#' @param y Chronological age
#' @param tit Plot title. Default is "Horvath's method".
#'
#' @export

plotDNAmAge <- function(x, y, tit = "Horvath's method", ...){
  df <- data.frame(x=x, y=y)
  my.formula <- y ~ x
  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_smooth(method = "lm", se=FALSE, color="black",
                         formula = my.formula) +
    ggplot2::xlab("DNA Methylation Age") +
    ggplot2::ylab("Chronological Age") +
    ggplot2::ggtitle(tit) +
    ggpmisc::stat_poly_eq(formula = my.formula,
                          ggplot2::aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")),
                          parse = TRUE) +
    ggplot2::geom_point()
  p
}
