#' methylclock
#' 
#' Package to estimate DNA methylation age (DNAmAge) using different methylation clocks. 
#' 
#' @docType package
#' @author Juan R Gonzalez <juanr.gonzalez@isglobal.org>
#' @import Rcpp
#' @importFrom Rcpp evalCpp
#' @importFrom RPMM blc betaObjf
#' @importFrom  minfi getBeta
#' @import ExperimentHub
#' @import dplyr
#' @import impute
#' @import PerformanceAnalytics
#' @import tidyverse
#' @import ggplot2
#' @import ggpubr 
#' @import tibble
#' @import preprocessCore
#' @import methylclockData
#' @useDynLib methylclock
#' @name methylclock
NULL  