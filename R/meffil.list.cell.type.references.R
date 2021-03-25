#' List of available cell type references
#'
#' @return a list with reference globals
#'
#' @details ORIGINAL AUTHOR: Matthew Suderman
#' The original meffil.list.cell.type.references and get.cell.type.reference function from meffil v1.0.0
#' at githug : https://github.com/perishky/meffil
#'
#' @examples
#' meffil.list.cell.type.references()
#' 
#' @export
meffil.list.cell.type.references <- function() {
  # ls(reference.globals)
  c("andrews and bakulski cord blood",
    "blood gse35069",
    "blood gse35069 chen",
    "blood gse35069 complete",
    "combined cord blood",
    "cord blood gse68456",
    "gervin and lyle cord blood",
    "guintivano dlpfc",
    "saliva gse48472")
}

#' Get cell type reference
#'
#' @return name and reference.globals
#' @examples
#' name <- "andrews and bakulski cord blood"
#' get.cell.type.reference(name)
#' 
#' @details ORIGINAL AUTHOR: Matthew Suderman
#' at githug : https://github.com/perishky/meffil
#' The original meffil.list.cell.type.references and get.cell.type.reference function from meffil v1.0.0
#'
get.cell.type.reference <- function(name) {
  stopifnot(is.character(name) && name %in% meffil.list.cell.type.references())
  get(name)
}
