#' Plot correlation among DNAm clockx
#' @param x a tible or data.frame with the different DNAm clocks
#' @param ... other arguments to be passs through function 'chart.Correlation' from 'PerformanceAnalytics' package
#'
#' @details To be supplied
#' 
#' @return Plot with Correlation Clocks
#'
#' @examples
#' dd <- GEOquery::getGEO("GSE109446")
#' gse109446 <- dd[[1]]
#' controls <- pData(gse109446)$`diagnosis:ch1` == "control"
#' gse <- gse109446[, controls]
#' age <- as.numeric(pData(gse)$`age:ch1`)
#' age.gse <- DNAmAge(gse, age = age)
#' plotCorClocks(age.gse)
#' 
#' @import ggplot2 ggpubr PerformanceAnalytics
#' @importFrom tidyr gather
#' @importFrom gridExtra grid.arrange
#' 
#' @export

plotCorClocks <- function(x, ...) {
  
  clocks <- c(
    "Horvath", "Levine", "BNN", "Horvath2", "Hannum", "PedBE", "skinHorvath",
    "Knigth", "Bohlin", "Mayne", "Lee", "BLUP", "EN" )
  
  # sel <- intersect(clocks, colnames(x))
  # x.sel <- x[, sel]
  # no.na <- apply(x.sel, 2, function(x) !all(is.na(x)))
  # x.nona <- x.sel[, no.na]
  # nclocks <- ncol(x.nona)
  
  res <- getNClockstoPlot(clocks, x)
  
  if( res$nclocks>=1 ) {
    ee <- tidyr::gather(res$x.nona, key = method, value = clock) %>% 
      add_column(age = rep(x$age, res$nclocks))
  
    p1 <- ggplot(ee, aes(x = clock, y = age)) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      xlab("DNAm clock") +
      ylab("Chronological Age") +
      ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
      facet_grid(~method)
  }
  
  
  clocks <- c( "TL")
  # sel <- intersect(clocks, colnames(x))
  # x.sel <- x[, sel]
  # no.na <- apply(x.sel, 2, function(x) !all(is.na(x)))
  # x.nona <- x.sel[, no.na]
  # nclocks <- ncol(x.nona)
  res <- getNClockstoPlot(clocks, x)
  if( res$nclocks>=1 ) {
    ee <- tidyr::gather(res$x.nona, key = method, value = clock) %>% 
      add_column(age = rep(x$age, res$nclocks))
    
    p2 <- ggplot(ee, aes(x = clock, y = age)) +
      geom_point() +
      geom_smooth(method = lm, se = FALSE) +
      xlab("Telomere Length (kb)") +
      ylab("Chronological Age") +
      ggpubr::stat_cor(aes(label = paste(..rr.label.., ..p.label.., sep = "~`,`~"))) +
      facet_grid(~method)
  } 
  
  if(exists("p1") && exists("p2")) {
    gridExtra::grid.arrange( p1, p2 )
  } else if (exists("p1")) {
    p1
  }else if (exists("p2")) {
    p2
  } else {
    message("No data to be plotted")
  } 
  
}



getNClockstoPlot <- function( clocks, data)
{
  
  sel <- intersect(clocks, colnames(data))
  data.sel <- data[, sel]
  no.na <- apply(data.sel, 2, function(data) !all(is.na(data)))
  data.nona <- data.sel[, no.na]
  nclocks <- ncol(data.nona)
  
  return(list( nclocks = nclocks,
               x.nona = data.nona) )
  
}  