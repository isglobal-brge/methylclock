#' DNAm age estimation using different DNA methylation clocks.
#' @param x data.frame (Individual in columns, CpGs in rows, CpG names in first colum - i.e. Horvath's format), matrix (individuals in columns and Cpgs in rows having CpG names in the rownames), ExpressionSet or GenomicRatioSet.
#' @param clocks the methods used for estimating DNAmAge. Currrently "Horvath", "Hannum", "Levine", "BNN", "Horvath2", "PedBE" and "all" are available. Default is "all" and all clocks are estimated.
#' @param toBetas Should data be transformed to beta values? Default is FALSE. If TRUE, it implies data are M values.
#' @param fastImp Is fast imputation performed if necessary? (see details). Default is FALSE
#' @param normalize Is Horvath's normalization performed? By default is FALSE
#' @param age individual's chronological age.
#' @param cell.count Are cell counts estimated? Default is TRUE.
#' @param cell.count.reference Used when 'cell.count' is TRUE. Default is "blood gse35069 complete". See 'meffil::meffil.list.cell.count.references()' for possible values.
#' @param min.perc Indicates the minimum conicidence percentage required between CpGs in or dataframee x and CpGs in clock coefficients to perform the calculation. If min.prec is too low, the estimated gestational DNAm age can be poor
#' @param ... Other arguments to be passed through impute package
#'
#' @details Imputation is performed when having missing data.
#'          Fast imputation is performed by ...
#'          what about imputing only when CpGs for the clock are missing?
#'
#' @examples
#' MethylationData <- read_csv(file.path(path, "MethylationDataExample55.csv"))
#' age.example55 <- DNAmAge(MethylationData)
#' 
#' @return The estimated chronological and biological mDNA age 
#' 
#' @import impute dplyr tidyverse tibble Rcpp
#' @importFrom Biobase featureNames exprs
#' @importFrom minfi getBeta
#' @export


DNAmAge <- function(x,
                    clocks = "all",
                    toBetas = FALSE,
                    fastImp = FALSE,
                    normalize = FALSE,
                    age,
                    cell.count = TRUE,
                    cell.count.reference = "blood gse35069 complete",
                    min.perc = 0.8,
                    ...) {
  available.clocks <- c("Horvath", "Hannum", "Levine", "BNN", "Horvath2", "PedBE", "Wu", "TL", "all")
  method <- match(clocks, available.clocks)
  if (any(is.na(method))) {
    stop("You wrote the name of an unavailable clock: Horvath, Hannum, Levine, BNN, Horvath2, PedBE, Wu, TL")
  }
  if (length(available.clocks) %in% method) {
    method <- c(1:(length(available.clocks) - 1))
  }

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
  }
  else if (inherits(x, "GenomicRatioSet")) {
    cpgs <- t(minfi::getBeta(x))
  }
  else {
    stop("x must be a data.frame or a 'GenomicRatioSet' or a 'ExpressionSet' object")
  }

  if (toBetas) {
    toBeta <- function(m) {
      2^m / (2^m + 1)
    }
    cpgs <- toBeta(cpgs)
  }

  if (any(cpgs < -0.1 | cpgs > 1.1, na.rm = TRUE)) {
    stop("Data seems to do not be beta values. Check your data or set 'toBetas=TRUE'")
  }


  cpgs.all <- c(
    coefHorvath$CpGmarker,
    coefHannum$CpGmarker,
    coefLevine$CpGmarker,
    coefSkin$CpGmarker,
    coefPedBE$CpGmarker,
    coefWu$CpGmarker,
    coefTL$CpGmarker
  )

  cpgs.in <- intersect(cpgs.all, colnames(cpgs))

  miss <- apply(cpgs[, cpgs.in], 2, function(x) any(is.na(x)))

  if (any(miss)) {
    if (fastImp) {
      cat(paste("Imputing missing data of", sum(miss), "CpGs .... \n"))
      mm <- apply(cpgs[, cpgs.in], 2, median, na.rm = TRUE)
      cpgs.imp <- sweep(cpgs[, cpgs.in], 2,
        STATS = mm,
        FUN = function(x, s) ifelse(is.na(x), s, x)
      )
    }
    else {
      quiet <- function(x) {
        sink(tempfile())
        on.exit(sink())
        invisible(force(x))
      }
      cat(paste("Imputing missing data of the entire matrix .... \n"))
      cpgs.imp <- quiet(t(impute.knn(t(cpgs), ...)$data))
    }
    cat("Data imputed. Starting DNAm clock estimation ... \n")
  }
  else {
    cpgs.imp <- cpgs
  }

  if (1 %in% method) {
    DNAmAge <- predAge(cpgs.imp, coefHorvath, intercept = TRUE, min.perc)
    horvath <- anti.trafo(DNAmAge)
    Horvath <- data.frame(
      id = rownames(cpgs.imp),
      Horvath = horvath
    )
  }


  if (2 %in% method) {
    hannum <- predAge(cpgs.imp, coefHannum, intercept = FALSE, min.perc)
    Hannum <- data.frame(
      id = rownames(cpgs.imp),
      Hannum = hannum
    )
  }

  if (3 %in% method) {
    levine <- predAge(cpgs.imp, coefLevine, intercept = TRUE, min.perc)
    Levine <- data.frame(
      id = rownames(cpgs.imp),
      Levine = levine
    )
  }


  if (4 %in% method) {
    if (any(!coefHorvath$CpGmarker[-1] %in% colnames(cpgs.imp))) {
      warning("Bayesian method cannot be estimated")
      bn <- rep(NA, nrow(cpgs.imp))
    }
    else {
      cpgs.bn <- t(cpgs.imp[, coefHorvath$CpGmarker[-1]])
      bn <- try(main_NewModel1Clean(cpgs.bn), TRUE)
      if (inherits(bn, "try-error")) {
        warning("Bayesian method produced an error")
        bn <- rep(NA, nrow(cpgs.imp))
      }
    }
    BNN <- data.frame(
      id = rownames(cpgs.imp),
      BNN = bn
    )
  }

  if (5 %in% method) {
    skinHorvath <- predAge(cpgs.imp, coefSkin, intercept = TRUE, min.perc)
    skinHorvath <- anti.trafo(skinHorvath)
    skinHorvath <- data.frame(
      id = rownames(cpgs.imp),
      skinHorvath = skinHorvath
    )
  }

  if (6 %in% method) {
    pedBE <- predAge(cpgs.imp, coefPedBE, intercept = TRUE, min.perc)
    pedBE <- anti.trafo(pedBE)
    PedBE <- data.frame(
      id = rownames(cpgs.imp),
      PedBE = pedBE
    )
  }

  if (7 %in% method) {
    wu <- predAge(cpgs.imp, coefWu, intercept = TRUE, min.perc)
    wu <- anti.trafo(wu)/12
    Wu <- data.frame(
      id = rownames(cpgs.imp),
      Wu = wu
    )
  }
  
  
  if (8 %in% method) {
    tl <- predAge(cpgs.imp, coefTL, intercept = TRUE, min.perc)
    TL <- data.frame(
      id = rownames(cpgs.imp),
      TL = tl
    )
  }

  if (!missing(age)) {
    if (!cell.count) {
      if (1 %in% method) {
        Horvath <- ageAcc1(Horvath, age, lab = "Horvath")
      }
      if (2 %in% method) {
        Hannum <- ageAcc1(Hannum, age, lab = "Hannum")
      }
      if (3 %in% method) {
        Levine <- ageAcc1(Levine, age, lab = "Levine")
      }
      if (4 %in% method) {
        BNN <- ageAcc1(BNN, age, lab = "BNN")
      }
      if (5 %in% method) {
        skinHorvath <- ageAcc1(skinHorvath, age, lab = "Horvath2")
      }
      if (6 %in% method) {
        PedBE <- ageAcc1(PedBE, age, lab = "PedBE")
      }
      if (7 %in% method) {
        Wu <- ageAcc1(Wu, age, lab = "Wu")
      }
      if (8 %in% method) {
        TL <- ageAcc1(TL, age, lab = "TL")
      }
    }
    else {
      cell.counts <- try(meffil.estimate.cell.counts.from.betas(
        t(cpgs), cell.count.reference), TRUE)

      if (inherits(cell.counts, "try-error")) {
        stop("cell counts cannot be estimated since meffil.estimate.cell.counts.from.betas function is giving an error.  
             Probably your data do not have any of the required CpGs for that reference panel.")
      } else {
        ok <- which(apply(cell.counts, 2, IQR) > 10e-6)
        cell.counts <- cell.counts[, ok]
        df <- data.frame(age = age, cell.counts)

        if (1 %in% method) {
          Horvath <- ageAcc2(Horvath, df, lab = "Horvath")
        }
        if (2 %in% method) {
          Hannum <- ageAcc2(Hannum, df, lab = "Hannum")
        }
        if (3 %in% method) {
          Levine <- ageAcc2(Levine, df, lab = "Levine")
        }
        if (4 %in% method) {
          BNN <- ageAcc2(BNN, df, lab = "BNN")
        }
        if (5 %in% method) {
          skinHorvath <- ageAcc2(skinHorvath, df, lab = "Horvath2")
        }
        if (6 %in% method) {
          PedBE <- ageAcc2(PedBE, df, lab = "PedBE")
        }
        if (7 %in% method) {
          Wu <- ageAcc2(Wu, df, lab = "Wu")
        }
        if (8 %in% method) {
          TL <- ageAcc2(TL, df, lab = "TL")
        }
      }
    }
  }
  else {
    cell.count <- FALSE
  }

  out <- NULL
  if (1 %in% method) {
    out <- Horvath
  }
  if (2 %in% method) {
    if(is.null(out)) {
      out <- Hannum
    } else {
      out <- out %>% full_join(Hannum, by = "id")
    }
  }
  if (3 %in% method) {
    if(is.null(out)) {
      out <- Levine
    } else{
      out <- out %>% full_join(Levine, by = "id")
    }
  }
  if (4 %in% method) {
    if(is.null(out)) {
      out <- BNN
    } else{
      out <- out %>% full_join(BNN, by = "id")
    }
  }
  if (5 %in% method) {
    if(is.null(out)) {
      out <- skinHorvath
    } else{
      out <- out %>% full_join(skinHorvath, by = "id")
    }
  }
  if (6 %in% method) {
    if(is.null(out)) {
      out <- PedBE
    } else{
      out <- out %>% full_join(PedBE, by = "id")
    }
  }
  if (7 %in% method) {
    if(is.null(out)) {
      out <- Wu
    } else{
      out <- out %>% full_join(Wu, by = "id")
    }
  }
  if (8 %in% method) {
    if(is.null(out)) {
      out <- TL
    } else{
      out <- out %>% full_join(TL, by = "id")
    }
  }

  out <- tibble::as_tibble(out)

  if (!missing(age)) {
    out <- add_column(out, age = age)
  }

  if (cell.count) {
    attr(out, "cell_proportion") <- cell.counts
  }

  out
}
