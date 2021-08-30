# Get CpGs name from input data
getInputCpgNames <- function(x) {
    if (inherits(x, "data.frame") & !inherits(x, c("tbl", "tbl_df"))) {
        cpgs.names <- x[, 1]
    } else if (inherits(x, "matrix")) {
        cpgs.names <- rownames(x)
    } else if (inherits(x, c("tbl", "tbl_df"))) {
        if (!"MethylationData" %in% ls(.GlobalEnv)) {
            MethylationData <- get_MethylationDataExample()
            assign("MethylationData", MethylationData, envir = .GlobalEnv)
        }
        cpgs.names <- pull(MethylationData, 1)
    } else if (inherits(x, "ExpressionSet")) {
        cpgs.names <- Biobase::featureNames(x)
    } else if (inherits(x, "GenomicRatioSet")) {
        cpgs.names <- Biobase::featureNames(x)
    }
    
    return(cpgs.names)
}



# Get CpGs values from input data
getInputCpgValues <- function(x, tobetas) {
    if (inherits(x, "data.frame")) {
        cpgs.names <- as.character(x[, 1, drop = TRUE])
        if (length(grep("cg", cpgs.names)) == 0) {
            stop("First column should contain CpG names")
        }
        cpgs <- t(as.matrix(x[, -1]))
        colnames(cpgs) <- cpgs.names
    }
    else if (inherits(x, "matrix")) {
        cpgs <- t(x)
    }
    else if (inherits(x, "ExpressionSet")) {
        cpgs <- t(Biobase::exprs(x))
        if(is.character(cpgs[1])) {
            colcpg <- colnames(cpgs)
            rowcpg <- rownames(cpgs)
            cpgs <- matrix(as.numeric(cpgs), ncol = length(colcpg))    
            rownames(cpgs) <- rowcpg
            colnames(cpgs) <- colcpg
        }
        
    }
    else if (inherits(x, "GenomicRatioSet")) {
        cpgs <- t(minfi::getBeta(x))
    }
    else {
        stop("x must be a data.frame, matrix, 'GenomicRatioSet' or
            an 'ExpressionSet' object")
    }
    
    
    if (tobetas) {
        toBeta <- function(m) {
            2^m / (2^m + 1)
        }
        cpgs <- toBeta(cpgs)
    }
    
    if (any(cpgs < -0.1 | cpgs > 1.1, na.rm = TRUE)) {
        stop("Data seems to do not be beta values. Check your data
            or set 'toBetas=TRUE'")
    }
    
    return(cpgs)
}
