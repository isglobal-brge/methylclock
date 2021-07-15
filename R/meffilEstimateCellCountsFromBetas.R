#' Estimate cell counts for a beta matrix from a reference
#'
#' Estimate cell type ratios from methylation profiles of purified cell 
#' populations (Infinium HumanMethylation450 BeadChip).
#'
#' @param beta Matrix of Illumina 450K methylation levels (rows = CpG sites, 
#' columns = subjects).
#' @param verbose If \code{TRUE}, then status messages are printed during 
#' execution
#' (Default: \code{FALSE}).
#' @param cellTypeReference Character string name of the cell type reference 
#' to use for estimating cell counts.
#' See \code{\link{meffil.list.cellTypeReferences}()} for a list of available 
#' references.  New references can be created using
#' @return A matrix of cell count estimates.
#'
#' Results should be nearly identical to estimateCellCounts() 
#' function in minfi package
#'  
#'
#' @details ORIGINAL AUTHOR: Matthew Suderman
#' The original meffil.list.cellTypeReferences and get.cellTypeReference 
#' function from meffil v1.0.0
#' downloaded from  githug : https://github.com/perishky/meffil
#'
#' @return betas
#' @examples
#'
#' cell.count.reference <- "andrews and bakulski cord blood"
#' TestDataset <- get_TestDataset()
#' cpgs <- t(as.matrix(TestDataset[, -1]))
#' colnames(cpgs) <- TestDataset$CpGName
#' meffilEstimateCellCountsFromBetas(t(cpgs), cell.count.reference)
#' @import preprocessCore quadprog devtools
#' @importFrom minfi getBeta
#'
#' @export
meffilEstimateCellCountsFromBetas <- function(beta, cellTypeReference,
                                                    verbose = FALSE) {
    stopifnot(is.matrix(beta))
    reference.object <- getCellTypeReference(cellTypeReference)

    beta <- quantile.normalize.betas(beta, reference.object$subsets,
                                        reference.object$quantiles,
                                        verbose = verbose )

    cpg.sites <- intersect(rownames(beta), rownames(reference.object$beta))
    stopifnot(length(cpg.sites) > 0)

    beta <- beta[cpg.sites, ]
    reference.beta <- reference.object$beta[cpg.sites, ]

    t(apply(beta, 2, estimateCellCountsFromBeta, reference.beta))
}


quantile.normalize.betas <- function(beta, subsets, 
                                    quantiles, verbose = FALSE) {

    stopifnot(is.matrix(beta))
    stopifnot(length(subsets) == length(quantiles))
    stopifnot(all(names(subsets) %in% names(quantiles)))

    for (subset.name in names(subsets)) {
        subset <- intersect(subsets[[subset.name]], rownames(beta))
        if (length(subset) == 0) {
            stop("subset", subset.name, "and beta matrix 
                have no features in common")
        }
        full.quantiles <- quantiles[[subset.name]]$beta
        full.quantiles <- approx( seq_len(length(full.quantiles)), 
                                full.quantiles, seq_len(length(subset)) )$y
        beta[subset, ] <- preprocessCore::normalize.quantiles.use.target(
                                beta[subset, , drop = FALSE], full.quantiles )
    }
    beta
}


## Input:
## beta[CpG] = methylation level of the CpG in the sample
## beta.cell.types[CpG,cell type] = methylation level of CpG in the cell type
##
## Output:
## counts[cell type] = proportion of cell type in the sample
## that minimizes ( beta - beta.cell.types * counts )^2
## subject to counts >= 0.
##
## Based on code from PMID 22568884.
estimateCellCountsFromBeta <- function(beta, beta.cell.types) {
    stopifnot(length(beta) == nrow(beta.cell.types))

    ## Let r = counts[cell type],
    ##     b = beta[cpg]
    ##     bc = beta.cell.types[cpg,cell type]
    ## for a given sample.
    ## We want to find r >= 0 that minimizes (b - bc r)^2.
    ## The unknown r is quadratic in the expression so
    ## we need a quadratic program solver.
    ##
    ## We use the function solve.QP(D,d,A,b0).
    ## It finds a vector b1 that minimizes
    ##   b1T D b1 - 2 dT b1 subject to AT b1 >= b0.
    ## We need to put (b - bc r)^2 into this form.
    ##
    ## (b - bc r)^2
    ## = (b - bc r)T (b - bc r)
    ## = bT b - 2 bT bc r - rT bcT bc r
    ## <=> finding r to minimize -2 bT bc r + rT (bcT bc) r
    ## = rT (bcT bc) r - 2 (bcT b)T r
    ## <=> solve.QP(bcT bc, bcT b, I, z) where I = identity matrix, z = 0

    I <- diag(ncol(beta.cell.types))
    zero <- rep(0, ncol(beta.cell.types))
    cpg.idx <- which(!is.na(beta))
    bcT.x.bc <- t(beta.cell.types[cpg.idx, ]) %*% beta.cell.types[cpg.idx, ]
    bcT.x.b <- t(beta.cell.types[cpg.idx, ]) %*% matrix(beta[cpg.idx],
                                                        nrow = length(cpg.idx))
    counts <- solve.QP(bcT.x.bc, bcT.x.b, I, zero)$sol
    names(counts) <- colnames(beta.cell.types)
    counts
}
