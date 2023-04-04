getCpGsData <- function(x) {
    
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
    else if (inherits(x, "SummarizedExperiment")) 
    {
        cpgs <- t(SummarizedExperiment::assay(x))
    }
    else {
        stop("x must be a data.frame or a 'GenomicRatioSet' or a 'ExpressionSet' or a 'SummarizedExperiment' object")
    }
    
    return(cpgs)
    
}


getCpGsFeatures <- function(x) {
    
    if (inherits(x, "data.frame") & !inherits(x, c("tbl", "tbl_df"))) {
        cpg.names <- x[, 1]
    } else if (inherits(x, "matrix")) {
        cpg.names <- rownames(x)
    } else if (inherits(x, c("tbl", "tbl_df"))) {
        cpg.names <- pull(MethylationData, 1)
    } else if (inherits(x, "ExpressionSet")) {
        cpg.names <- Biobase::featureNames(x)
    } else if (inherits(x, "GenomicRatioSet")) {
        cpg.names <- Biobase::featureNames(x)
    } else if (inherits(x, "SummarizedExperiment")) {
        cpg.names <-  SummarizedExperiment::rownames(x)
    }
    
    return(cpg.names)
}


ageAcc1 <- function(x, age, lab) {
  y <- x[, 2]
  if (all(is.na(y))) {
    ans <- x
  } else {
    mod.ieaa <- lm(y ~ age, na.action = "na.exclude")
    ans <- data.frame(x,
      ageAcc = y - age,
      ageAcc2 = resid(mod.ieaa)
    )
    names(ans)[3:4] <- paste(names(ans)[3:4], lab, sep = ".")
  }
  ans
}

ageAcc2 <- function(x, df, lab) {
  y <- x[, 2]
  if (all(is.na(y))) {
    ans <- x
  } else {
    mod.ieaa <- lm(y ~ age, data = df, na.action = "na.exclude")
    mod.eeaa <- lm(y ~ ., data = df, na.action = "na.exclude")
    ans <- data.frame(x,
      ageAcc = y - df$age,
      ageAcc2 = resid(mod.ieaa),
      ageAcc3 = resid(mod.eeaa)
    )
    names(ans)[3:5] <- paste(names(ans)[3:5], lab, sep = ".")
  }
  ans
}


predAge <- function(x, coefs, intercept = TRUE, min.perc = 0.8) {
  
  if(inherits(coefs$CpGmarker, "factor")){
      coefs$CpGmarker <- as.character(coefs$CpGmarker)
  }
    
  cpgs <- colnames(x)
  mask <- coefs$CpGmarker %in% cpgs

  if (mean(mask) > min.perc) {
    obs.cpgs <- coefs$CpGmarker[mask]
    X <- x[, obs.cpgs]
    predAge <- X %*% coefs$CoefficientTraining[coefs$CpGmarker %in% obs.cpgs]
    if (intercept) {
      predAge <- coefs$CoefficientTraining[1] + predAge
    }
  }
  else {
    tit <- gsub("GA", "", gsub("coef", "", substitute(coefs)))
    warning(paste("The number of missing CpGs for", tit, "clock exceeds ",min.perc*100,"%.\n  ---> This DNAm clock will be NA.\n"))
    predAge <- rep(NA, nrow(x))
  }
  predAge
}



predAgeDunedin <- function(x, coefs, coefsgs, intercept = TRUE, min.perc = 0.8) {
    
    if(inherits(coefs$CpGmarker, "factor")){
        coefs$CpGmarker <- as.character(coefs$CpGmarker)
    }
    
    intercept <- coefs[1,]
    coefs <- coefs[-1,]
    cpgs <- colnames(x)
    mask <- coefs$CpGmarker %in% cpgs
    
    
    if (mean(mask) > min.perc) {
        obs.cpgs <- coefsgs$CpGmarker[mask]
        X <- x[, obs.cpgs]

        toAdd <- coefsgs$CpGmarker[which(!coefsgs$CpGmarker %in% colnames(X))]
        
        sapply(toAdd, function(probe) {
            X <- rbind( X, probe = rep(coefsgs$MeansGS[probe], ncol(X)))
            colnames(X)[which(colnames(X) == "probe")] == probe
        })
        
        # Remove samples with a lot of missing values
        toRemove <- rownames(X)[which(apply(X, 1, function(sx) { 1 - ( length(which(is.na(sx))) / length(sx) ) < min.perc}))]
        if( length(toRemove) > 0 ) {
            X <- X[ -which(rownames(X) %in% toRemove),]
        }
        
        X <- t(X)
        
        if(ncol(X) > 0) {
            # Identify missingness on a probe level
            pct_vp <- apply( X, 1, function(sx) { 1 - (length(which(is.na(sx))) / length(sx)) } )
            # If they're missing values, but less than the proportion required, we impute to the cohort mean
            toAdjust <- which(pct_vp < 1 & pct_vp >= min.perc)
            if( length(toAdjust) > 0 ) {
                if( length(toAdjust) > 1 ) {
                    X[toAdjust,] <- t(apply( X[toAdjust,], 1 , function(X) {
                        X[is.na(X)] = mean( X, na.rm = TRUE )
                        X
                    }))
                } else {
                    X[toAdjust,which(is.na(X[toAdjust,]))] <- mean(X[toAdjust,], na.rm=T)
                }
            }
            # If they're missing too many values, everyones value gets replaced with the mean from the Dunedin cohort
            if( length(which(pct_vp < min.perc)) > 0 ) {
                toReplace <- rownames(X)[which(pct_vp < min.perc)]
                sapply( toReplace, function(prb) {
                    X[prb,] <- rep(coefs$Means[prb], ncol(X))
                })
            }
            
            # Normalize the matrix to the gold standard dataset
            X.norm <- preprocessCore::normalize.quantiles.use.target(X, target=coefsgs$MeansGS)
            rownames(X.norm) <- rownames(X)
            colnames(X.norm) <- colnames(X)
            # Calculate score:
            score = intercept$CoefficientTraining + rowSums(t(X.norm[coefs$CpGmarker,]) %*% diag(coefs$CoefficientTraining))
            names(score) <- colnames(X.norm)
            if( length(toRemove) > 0 ) {
                score.tmp <- rep(NA, length(toRemove))
                names(score.tmp) <- toRemove
                score <- c(score, score.tmp)
            }
            
            predAge <- score[rownames(x)]
        } else {
            predAge <- rep(NA, ncol(X))
            names(predAge) <- rownames(X)
        }
        
    }
    else {
        tit <- gsub("GA", "", gsub("coef", "", substitute(coefs)))
        warning(paste("The number of missing CpGs for", tit, "clock exceeds ",min.perc*100,"%.\n  ---> This DNAm clock will be NA.\n"))
        predAge <- rep(NA, nrow(x))
    }
    return(as.data.frame(predAge))
}



trafo <- function(x, adult.age = 20) {
  x <- (x + 1) / (1 + adult.age)
  y <- ifelse(x <= 1, log(x), x - 1)
  y
}

anti.trafo <- function(x, adult.age = 20) {
  ifelse(x < 0, (1 + adult.age) * exp(x) - 1, (1 + adult.age) * x + adult.age)
}


betaEst2 <- function(y, w, weights) {
  yobs <- !is.na(y)
  if (sum(yobs) <= 1) {
    return(c(1, 1))
  }
  y <- y[yobs]
  w <- w[yobs]
  weights <- weights[yobs]
  N <- sum(weights * w)
  p <- sum(weights * w * y) / N
  v <- sum(weights * w * y * y) / N - p * p
  logab <- log(c(p, 1 - p)) + log(pmax(1e-06, p * (1 - p) / v -
    1))
  if (sum(yobs) == 2) {
    return(exp(logab))
  }
  opt <- try(optim(
    logab,
    betaObjf,
    ydata = y,
    wdata = w,
    weights = weights,
    method = "Nelder-Mead",
    control = list(maxit = 50)
  ),
  silent = TRUE
  )
  if (inherits(opt, "try-error")) {
    return(c(1, 1))
  }
  exp(opt$par)
} # end of function betaEst



blc2 <- function(Y,
                 w,
                 maxiter = 25,
                 tol = 1e-06,
                 weights = NULL,
                 verbose = TRUE) {
  Ymn <- min(Y[Y > 0], na.rm = TRUE)
  Ymx <- max(Y[Y < 1], na.rm = TRUE)
  Y <- pmax(Y, Ymn / 2)
  Y <- pmin(Y, 1 - (1 - Ymx) / 2)
  Yobs <- !is.na(Y)
  J <- dim(Y)[2]
  K <- dim(w)[2]
  n <- dim(w)[1]
  if (n != dim(Y)[1]) {
    stop("Dimensions of w and Y do not agree")
  }
  if (is.null(weights)) {
    weights <- rep(1, n)
  }
  mu <- a <- b <- matrix(Inf, K, J)
  crit <- Inf
  for (i in 1:maxiter) {
    warn0 <- options()$warn
    options(warn = -1)
    eta <- apply(weights * w, 2, sum) / sum(weights)
    mu0 <- mu
    for (k in 1:K) {
      for (j in 1:J) {
        ab <- betaEst2(Y[, j], w[, k], weights)
        a[k, j] <- ab[1]
        b[k, j] <- ab[2]
        mu[k, j] <- ab[1] / sum(ab)
      }
    }
    ww <- array(0, dim = c(n, J, K))
    for (k in 1:K) {
      for (j in 1:J) {
        ww[Yobs[, j], j, k] <- dbeta(Y[Yobs[, j], j],
          a[k, j], b[k, j],
          log = TRUE
        )
      }
    }
    options(warn = warn0)
    w <- apply(ww, c(1, 3), sum, na.rm = TRUE)
    wmax <- apply(w, 1, max)
    for (k in 1:K) {
      w[, k] <- w[, k] - wmax
    }
    w <- t(eta * t(exp(w)))
    like <- apply(w, 1, sum)
    w <- (1 / like) * w
    llike <- weights * (log(like) + wmax)
    crit <- max(abs(mu - mu0))
    if (verbose) {
      print(crit)
    }
    if (crit < tol) {
      break
    }
  }
  return(list(
    a = a,
    b = b,
    eta = eta,
    mu = mu,
    w = w,
    llike = sum(llike)
  ))
}

CheckBMIQ <- function(beta.v, design.v, pnbeta.v) {
  ### pnbeta is BMIQ normalised profile
  type1.idx <- which(design.v == 1)
  type2.idx <- which(design.v == 2)
  beta1.v <- beta.v[type1.idx]
  beta2.v <- beta.v[type2.idx]
  pnbeta2.v <- pnbeta.v[type2.idx]
} # end of function CheckBMIQ

CalibrateUnitInterval <- function(datM, onlyIfOutside = TRUE) {
  rangeBySample <- data.frame(lapply(data.frame(t(datM)), range, na.rm = TRUE))
  minBySample <- as.numeric(rangeBySample[1, ])
  maxBySample <- as.numeric(rangeBySample[2, ])
  if (onlyIfOutside) {
    indexSamples <- which((minBySample < 0 |
      maxBySample > 1) & !is.na(minBySample) & !is.na(maxBySample))
  }
  if (!onlyIfOutside) {
    indexSamples <- 1:length(minBySample)
  }
  if (length(indexSamples) >= 1) {
    for (i in indexSamples) {
      y1 <- c(0.001, 0.999)
      x1 <- c(minBySample[i], maxBySample[i])
      lm1 <- lm(y1 ~ x1)
      intercept1 <- coef(lm1)[[1]]
      slope1 <- coef(lm1)[[2]]
      datM[i, ] <- intercept1 + slope1 * datM[i, ]
    } # end of for loop
  }
  datM
} # end of function for calibrating to [0,1]


cpgs_imputation <- function( imp, cpgs, fastImp, ...)
{
  
  cpgs.imp <- NULL
  
  if (any(imp)) {
    if (fastImp) {
      cat(paste("Imputing missing data of", sum(imp), "CpGs .... \n"))
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
  
  return(cpgs.imp)
}