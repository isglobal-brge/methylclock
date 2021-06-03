# Load data from methylclockData package
#' @export 
load_DNAmGA_Clocks_data <- function() 
{
  
  if(!"coefKnightGA" %in%  ls()) {
    coefKnightGA <- get_coefKnightGA()
    assign("coefKnightGA", coefKnightGA, envir=.GlobalEnv)
  }
  if(!"coefBohlin" %in%  ls()) {
    coefBohlin <- get_coefBohlin()
    assign("coefBohlin", coefBohlin, envir=.GlobalEnv)
  }
  if(!"coefMayneGA" %in%  ls()) {
    coefMayneGA <- get_coefMayneGA()
    assign("coefMayneGA", coefMayneGA, envir=.GlobalEnv)
  }
  if(!"coefLeeGA" %in%  ls()) {
    coefLeeGA <- get_coefLeeGA()
    assign("coefLeeGA", coefLeeGA, envir=.GlobalEnv)
  }
  
}