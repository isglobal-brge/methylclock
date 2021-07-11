#' List of available cell type references
#'
#' @return a list with reference globals
#'
#' @details ORIGINAL AUTHOR: Matthew Suderman
#' The original meffilListCellTypeReferences and 
#' getCellTypeReference function from meffil v1.0.0
#' at githug : https://github.com/perishky/meffil
#'
#' @examples
#' meffilListCellTypeReferences()
#' 
#' @export
meffilListCellTypeReferences <- function() {
  # ls(reference.globals)
  # Get data from methylclockData package
  if( !all( c("andrews and bakulski cord blood",
                   "blood gse35069",
                   "blood gse35069 chen",
                   "blood gse35069 complete",
                   "combined cord blood",
                   "cord blood gse68456",
                   "gervin and lyle cord blood",
                   "guintivano dlpfc",
                   "saliva gse48472") %in% ls(.GlobalEnv)))
  {
    references <- get_references()
    refs <- load(references)
    lapply(refs, function(x) assign(eval(x), get(x), envir = .GlobalEnv))
  }
  return ( ls(.GlobalEnv) )
}

#' Get cell type reference
#'
#' @param name, string with predefined datasets andrews and bakulski cord
#' blood, blood gse35069, blood gse35069 chen, blood gse35069 complete,
#' "combined cord blood", "cord blood gse68456", "gervin and lyle cord blood",
#' "guintivano dlpfc" or "saliva gse48472"
#' @return name and reference.globals
#' @examples
#' name <- "andrews and bakulski cord blood"
#' getCellTypeReference(name)
#' 
#' @details ORIGINAL AUTHOR: Matthew Suderman
#' at githug : https://github.com/perishky/meffil
#' The original meffilListCellTypeReferences and 
#' getCellTypeReference function from meffil v1.0.0
#' @export
getCellTypeReference <- function(name) {
  stopifnot(is.character(name) && name %in% meffilListCellTypeReferences())
  get(name, envir = .GlobalEnv)
}
