#' Plot DNAm age estimation vs chronological age.
#' @param x DNAm age estimation
#' @param y Chronological age
#' @param clock Type of clock 'chronological' or 'GA'. Default is 'chronological'.
#' @param tit Plot title. Default is "Horvath's method".
#' @param ... Other plot parameters for ggplot
#'
#' @return Plot with estimated DNAmAge
#'
#' @examples
#' MethylationData <- read_csv(file.path(path, "MethylationDataExample55.csv"))
#' age.example55 <- DNAmAge(MethylationData)
#' plotDNAmAge(age.example55$Horvath, age)
#' @import ggplot2
#' @importFrom ggpmisc stat_poly_eq
#' @export

plotDNAmAge <- function(x, y, tit = "Horvath's method", clock = "chronological", ...) {
  df <- data.frame(x = x, y = y)
  my.formula <- y ~ x
  if (missing(clock)) {
    yy <- "Chronological Age"
  } else if (clock == "GA") {
    yy <- "Gestational Age"
  } else if (clock == "TL"){
    yy <- "Age"
  }
  
  if( clock != 'TL'){

    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, color = "black",
        formula = my.formula
      ) +
      ggplot2::xlab("DNA Methylation Age") +
      ggplot2::ylab(yy) +
      ggplot2::ggtitle(tit) +
      ggpmisc::stat_poly_eq(
        formula = my.formula,
        ggplot2::aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
        parse = TRUE
      ) +
      ggplot2::geom_point()
    p
  } else {
    
    p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y)) +
      ggplot2::geom_smooth(
        method = "lm", se = FALSE, color = "black",
        formula = my.formula
      ) +
      ggplot2::xlab("Methylation - Telomere Length (kb)") +
      ggplot2::ylab(yy) +
      ggplot2::ggtitle(tit) +
      ggpmisc::stat_poly_eq(
        formula = my.formula,
        ggplot2::aes(label = paste(after_stat(eq.label), after_stat(rr.label), sep = "~~~")),
        parse = TRUE
      ) +
      ggplot2::geom_point()
    p
  }
    
  
  
}
