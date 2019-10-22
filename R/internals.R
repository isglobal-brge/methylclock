ageAcc1 <- function(x, age, lab){
  y <- x[,2]
  if (all(is.na(y)))
    ans <- x
  else {
    mod.ieaa <- lm(y ~ age, na.action="na.exclude")
    ans <- data.frame(x,
                    ageAcc = y-age,
                    ageAcc2 = resid(mod.ieaa))
    names(ans)[3:4] <- paste(names(ans)[3:4], lab, sep=".")
  }
  ans
}

ageAcc2 <- function(x, df, lab){
  y <- x[,2]
  if (all(is.na(y)))
    ans <- x
  else{
    mod.ieaa <- lm(y ~ age, na.action="na.exclude")
    mod.eeaa <- lm(y ~ ., data=df, na.action="na.exclude")
    ans <- data.frame(x,
                    ageAcc = y-age,
                    ageAcc2 = resid(mod.ieaa),
                    ageAcc3 = resid(mod.eeaa))
    names(ans)[3:5] <- paste(names(ans)[3:5], lab, sep=".")
    }
  ans
}


predAge <- function(x, coefs, intercept=TRUE){
  cpgs <- colnames(x)
  if (intercept)
   mask <- coefs$CpGmarker%in%cpgs
  else
    mask <- coefs$CpGmarker[-1]%in%cpgs
  if (mean(mask)>0.8){
    if (intercept)
      obs.cpgs <- coefs$CpGmarker[mask]
    else
      obs.cpgs <- coefs$CpGmarker[-1][mask]  
    X <- x[, obs.cpgs]
    predAge <- X%*%coefs$CoefficientTraining[coefs$CpGmarker%in%obs.cpgs]
    if (intercept)
      predAge <- coefs$CoefficientTraining[1] + predAge
  }
  else{
    tit <- gsub("GA", "", gsub("coef", "", substitute(coefs)))
    warning(paste("The number of missing CpGs for", tit , "clock exceeds 80%.\n  ---> This DNAm clock will be NA.\n"))
    predAge <- rep(NA, nrow(x))
  }
  predAge
}



trafo = function(x, adult.age = 20) {
  x = (x + 1) / (1 + adult.age)
  y = ifelse(x <= 1, log(x), x - 1)
  y
}

anti.trafo = function(x, adult.age = 20) {
  ifelse(x < 0, (1 + adult.age) * exp(x) - 1, (1 + adult.age) * x + adult.age)
}


betaEst2 <- function (y, w, weights)
{
  yobs = !is.na(y)
  if (sum(yobs) <= 1)
    return(c(1, 1))
  y = y[yobs]
  w = w[yobs]
  weights = weights[yobs]
  N = sum(weights * w)
  p = sum(weights * w * y) / N
  v = sum(weights * w * y * y) / N - p * p
  logab = log(c(p, 1 - p)) + log(pmax(1e-06, p * (1 - p) / v -
                                        1))
  if (sum(yobs) == 2)
    return(exp(logab))
  opt = try(optim(
    logab,
    betaObjf,
    ydata = y,
    wdata = w,
    weights = weights,
    method = "Nelder-Mead",
    control = list(maxit = 50)
  ),
  silent = TRUE)
  if (inherits(opt, "try-error"))
    return(c(1, 1))
  exp(opt$par)
} # end of function betaEst



blc2 <- function (Y,
                  w,
                  maxiter = 25,
                  tol = 1e-06,
                  weights = NULL,
                  verbose = TRUE)
{
  Ymn = min(Y[Y > 0], na.rm = TRUE)
  Ymx = max(Y[Y < 1], na.rm = TRUE)
  Y = pmax(Y, Ymn / 2)
  Y = pmin(Y, 1 - (1 - Ymx) / 2)
  Yobs = !is.na(Y)
  J = dim(Y)[2]
  K = dim(w)[2]
  n = dim(w)[1]
  if (n != dim(Y)[1])
    stop("Dimensions of w and Y do not agree")
  if (is.null(weights))
    weights = rep(1, n)
  mu = a = b = matrix(Inf, K, J)
  crit = Inf
  for (i in 1:maxiter) {
    warn0 = options()$warn
    options(warn = -1)
    eta = apply(weights * w, 2, sum) / sum(weights)
    mu0 = mu
    for (k in 1:K) {
      for (j in 1:J) {
        ab = betaEst2(Y[, j], w[, k], weights)
        a[k, j] = ab[1]
        b[k, j] = ab[2]
        mu[k, j] = ab[1] / sum(ab)
      }
    }
    ww = array(0, dim = c(n, J, K))
    for (k in 1:K) {
      for (j in 1:J) {
        ww[Yobs[, j], j, k] = dbeta(Y[Yobs[, j], j],
                                    a[k, j], b[k, j], log = TRUE)
      }
    }
    options(warn = warn0)
    w = apply(ww, c(1, 3), sum, na.rm = TRUE)
    wmax = apply(w, 1, max)
    for (k in 1:K)
      w[, k] = w[, k] - wmax
    w = t(eta * t(exp(w)))
    like = apply(w, 1, sum)
    w = (1 / like) * w
    llike = weights * (log(like) + wmax)
    crit = max(abs(mu - mu0))
    if (verbose)
      print(crit)
    if (crit < tol)
      break
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
  type1.idx = which(design.v == 1)
  type2.idx = which(design.v == 2)
  beta1.v = beta.v[type1.idx]
  beta2.v = beta.v[type2.idx]
  pnbeta2.v = pnbeta.v[type2.idx]
} # end of function CheckBMIQ

CalibrateUnitInterval = function(datM, onlyIfOutside = TRUE) {
  rangeBySample = data.frame(lapply(data.frame(t(datM)), range, na.rm = TRUE))
  minBySample = as.numeric(rangeBySample[1, ])
  maxBySample = as.numeric(rangeBySample[2, ])
  if (onlyIfOutside) {
    indexSamples = which((minBySample < 0 |
                            maxBySample > 1) & !is.na(minBySample) & !is.na(maxBySample))
  }
  if (!onlyIfOutside) {
    indexSamples = 1:length(minBySample)
  }
  if (length(indexSamples) >= 1) {
    for (i in indexSamples) {
      y1 = c(0.001, 0.999)
      x1 = c(minBySample[i], maxBySample[i])
      lm1 = lm(y1 ~ x1)
      intercept1 = coef(lm1)[[1]]
      slope1 = coef(lm1)[[2]]
      datM[i, ] = intercept1 + slope1 * datM[i, ]
    } # end of for loop
  }
  datM
} #end of function for calibrating to [0,1]
