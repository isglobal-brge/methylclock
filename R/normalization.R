#' BMIQ normallization
#'
#' @param datM data matrix
#' @param goldstandard.beta 'goldstandard.beta'
#' @param nL default value is 3
#' @param doH default value is TRUE
#' @param nfit devault value id 20000
#' @param th1.v default value is c(0.2, 0.75)
#' @param th2.v default valued is NULL
#' @param niter default value is 5
#' @param tol defalt value is 0.001
#' @param plots default value is FALE
#' @param calibrateUnitInterval default value is TRUE
#' @return the normalized table and a plot when plot is equal to TRUE
#' @details ORIGINAL AUTHOR: Andrew Teschendorff
#' The original BMIQ function from Teschendorff 2013 adjusts for the
#' type-2 bias in Illumina Infinium 450k data.
#' Later functions and edits were provided by yours truly, Steve Horvath.
#' I changed the code so that one can calibrate methylation data to a gold standard.
#' Specifically, I took version v_1.2 by Teschendorff  and fixed minor issues.
#' Also I made the code more robust e.g. by changing the optimization algorithm.
#' Toward this end, I used the method="Nelder-Mead" in optim()
#'
#' Later functions and edits by Steve Horvath
#' Steve Horvath took version v_1.2 by Teschendorff
#' and fixed minor errors. Also he made the code more robust.
#' Importantly, SH changed the optimization algorithm to make it more robust.
#' SH used method="Nelder-Mead" in optim() since the other optimization method
#' sometimes gets stuck. Toward this end, the function blc was replaced by blc2.
#' 
#' @examples
#' \dontrun{
#' goldstandard.beta <- c(2.4,0.1,0.01,0.001)
#' TestDataset <- get_TestDataset()
#' methylclock:::BMIQcalibration(TestDataset[1:50,], goldstandard.beta)
#' }
#' 

BMIQcalibration <- function(datM,
                            goldstandard.beta,
                            nL = 3,
                            doH = TRUE,
                            nfit = 20000,
                            th1.v = c(0.2, 0.75),
                            th2.v = NULL,
                            niter = 5,
                            tol = 0.001,
                            plots = FALSE,
                            calibrateUnitInterval = TRUE) {
  # Fixed JRG Oct'18
  #  if (length(goldstandard.beta) != dim(datM)[[2]]) {
  if (length(goldstandard.beta) > dim(datM)[[2]]) {
    stop(
      "Error in function arguments length(goldstandard.beta) !=dim(datM)[[2]]. Consider transposing datM."
    )
  }
  if (plots) {
    par(mfrow = c(2, 2))
  }
  beta1.v <- goldstandard.beta

  if (calibrateUnitInterval) {
    datM <- CalibrateUnitInterval(datM)
  }

  ### estimate initial weight matrix from type1 distribution
  w0.m <- matrix(0, nrow = length(beta1.v), ncol = nL)

  w0.m[which(beta1.v <= th1.v[1]), 1] <- 1

  w0.m[intersect(which(beta1.v > th1.v[1]), which(beta1.v <= th1.v[2])), 2] <- 1

  w0.m[which(beta1.v > th1.v[2]), 3] <- 1

  ### fit type1
  message("Fitting EM beta mixture to goldstandard probes")

  ## Disabled to accomplish BioC check() 
  ## set.seed(1)
  rand.idx <- sample(1:length(beta1.v), min(c(nfit, length(beta1.v))),
                     #..# rand.idx <- sample(seq_len(beta1.v), min(c(nfit, length(beta1.v))),
    replace = FALSE
  )
  em1.o <- blc(
    matrix(beta1.v[rand.idx], ncol = 1),
    w = w0.m[rand.idx, ],
    maxiter = niter,
    tol = tol
  )

  subsetclass1.v <- apply(em1.o$w, 1, which.max)

  subsetth1.v <- c(mean(max(beta1.v[rand.idx[subsetclass1.v == 1]]), min(beta1.v[rand.idx[subsetclass1.v ==
    2]])), mean(max(beta1.v[rand.idx[subsetclass1.v == 2]]), min(beta1.v[rand.idx[subsetclass1.v ==
    3]])))

  class1.v <- rep(2, length(beta1.v))

  class1.v[which(beta1.v < subsetth1.v[1])] <- 1

  class1.v[which(beta1.v > subsetth1.v[2])] <- 3

  nth1.v <- subsetth1.v

  message("Done")


  ### generate plot from estimated mixture
  if (plots) {
    message("Check")

    tmpL.v <- as.vector(rmultinom(1:nL, length(beta1.v), prob = em1.o$eta))

    tmpB.v <- vector()

    for (l in 1:nL) {
    #..# for (l in seq_len(nL)) {
      tmpB.v <- c(tmpB.v, rbeta(tmpL.v[l], em1.o$a[l, 1], em1.o$b[l, 1]))
    }
    plot(density(beta1.v), main = paste("Type1fit-", sep = ""))

    d.o <- density(tmpB.v)

    points(d.o$x, d.o$y, col = "green", type = "l")
    legend(
      x = 0.5,
      y = 3,
      legend = c("obs", "fit"),
      fill = c("black", "green"),
      bty = "n"
    )
  }

  ### Estimate Modes
  if (sum(class1.v == 1) == 1) {
    mod1U <- beta1.v[class1.v == 1]
  }
  if (sum(class1.v == 3) == 1) {
    mod1M <- beta1.v[class1.v == 3]
  }
  if (sum(class1.v == 1) > 1) {
    d1U.o <- density(beta1.v[class1.v == 1])
    mod1U <- d1U.o$x[which.max(d1U.o$y)]
  }
  if (sum(class1.v == 3) > 1) {
    d1M.o <- density(beta1.v[class1.v == 3])
    mod1M <- d1M.o$x[which.max(d1M.o$y)]
  }

  ### BETA 2
  for (ii in 1:dim(datM)[[1]]) {
    printFlush(paste("ii=", ii))
    sampleID <- ii
    beta2.v <- as.numeric(datM[ii, ])

    d2U.o <- density(beta2.v[which(beta2.v < 0.4)])

    d2M.o <- density(beta2.v[which(beta2.v > 0.6)])

    mod2U <- d2U.o$x[which.max(d2U.o$y)]
    mod2M <- d2M.o$x[which.max(d2M.o$y)]

    ### now deal with type2 fit
    th2.v <- vector()

    th2.v[1] <- nth1.v[1] + (mod2U - mod1U)

    th2.v[2] <- nth1.v[2] + (mod2M - mod1M)


    ### estimate initial weight matrix
    w0.m <- matrix(0, nrow = length(beta2.v), ncol = nL)

    w0.m[which(beta2.v <= th2.v[1]), 1] <- 1

    w0.m[intersect(which(beta2.v > th2.v[1]), which(beta2.v <= th2.v[2])), 2] <- 1

    w0.m[which(beta2.v > th2.v[2]), 3] <- 1


    message("Fitting EM beta mixture to input probes")
    # incProgress(1/dim(datM)[[1]])

    # I fixed an error in the following line (replaced beta1 by beta2)
    ## Disabled to accomplish BioC check() 
    ## set.seed(1)
    rand.idx <- sample(1:length(beta2.v), min(c(nfit, length(beta2.v)),
    #..# rand.idx <- sample(seq_len(beta2.v), min(c(nfit, length(beta2.v)),
      na.rm =
        TRUE
    ), replace = FALSE)
    em2.o <- blc2(
      Y = matrix(beta2.v[rand.idx], ncol = 1),
      w = w0.m[rand.idx, ],
      maxiter = niter,
      tol = tol,
      verbose = TRUE
    )

    message("Done")


    ### for type II probes assign to state (unmethylated, hemi or full methylation)
    subsetclass2.v <- apply(em2.o$w, 1, which.max)



    if (sum(subsetclass2.v == 2) > 0) {
      subsetth2.v <- c(
        mean(max(beta2.v[rand.idx[subsetclass2.v == 1]]), min(beta2.v[rand.idx[subsetclass2.v ==
          2]])),
        mean(max(beta2.v[rand.idx[subsetclass2.v == 2]]), min(beta2.v[rand.idx[subsetclass2.v ==
          3]]))
      )
    }
    if (sum(subsetclass2.v == 2) == 0) {
      subsetth2.v <- c(
        1 / 2 * max(beta2.v[rand.idx[subsetclass2.v == 1]]) + 1 / 2 * mean(beta2.v[rand.idx[subsetclass2.v ==
          3]]),
        1 / 3 * max(beta2.v[rand.idx[subsetclass2.v == 1]]) + 2 / 3 * mean(beta2.v[rand.idx[subsetclass2.v ==
          3]])
      )
    }



    class2.v <- rep(2, length(beta2.v))

    class2.v[which(beta2.v <= subsetth2.v[1])] <- 1

    class2.v[which(beta2.v >= subsetth2.v[2])] <- 3


    ### generate plot
    if (plots) {
      tmpL.v <- as.vector(rmultinom(1:nL, length(beta2.v), prob = em2.o$eta))
      #..# tmpL.v <- as.vector(rmultinom( seq_len(nL), length(beta2.v), prob = em2.o$eta))

      tmpB.v <- vector()

      for (lt in 1:nL) {
      #..#for (lt in seq_len(nL)) {
        tmpB.v <- c(tmpB.v, rbeta(tmpL.v[lt], em2.o$a[lt, 1], em2.o$b[lt, 1]))
      }
      plot(density(beta2.v), main = paste("Type2fit-", sampleID, sep = ""))

      d.o <- density(tmpB.v)

      points(d.o$x, d.o$y, col = "green", type = "l")
      legend(
        x = 0.5,
        y = 3,
        legend = c("obs", "fit"),
        fill = c("black", "green"),
        bty = "n"
      )
    }

    classAV1.v <- vector()
    classAV2.v <- vector()

    for (l in 1:nL) {
    #..#for (l in seq_len(nL)) {
      classAV1.v[l] <- em1.o$mu[l, 1]

      classAV2.v[l] <- em2.o$mu[l, 1]
    }

    ### start normalising input probes
    message("Start normalising input probes")

    nbeta2.v <- beta2.v

    ### select U probes
    lt <- 1

    selU.idx <- which(class2.v == lt)

    selUR.idx <- selU.idx[which(beta2.v[selU.idx] > classAV2.v[lt])]

    selUL.idx <- selU.idx[which(beta2.v[selU.idx] < classAV2.v[lt])]

    ### find prob according to typeII distribution
    p.v <- pbeta(beta2.v[selUR.idx], em2.o$a[lt, 1], em2.o$b[lt, 1],
      lower.tail =
        FALSE
    )

    ### find corresponding quantile in type I distribution
    q.v <- qbeta(p.v, em1.o$a[lt, 1], em1.o$b[lt, 1], lower.tail = FALSE)

    nbeta2.v[selUR.idx] <- q.v

    p.v <- pbeta(beta2.v[selUL.idx], em2.o$a[lt, 1], em2.o$b[lt, 1],
      lower.tail =
        TRUE
    )

    ### find corresponding quantile in type I distribution
    q.v <- qbeta(p.v, em1.o$a[lt, 1], em1.o$b[lt, 1], lower.tail = TRUE)

    nbeta2.v[selUL.idx] <- q.v


    ### select M probes
    lt <- 3

    selM.idx <- which(class2.v == lt)

    selMR.idx <- selM.idx[which(beta2.v[selM.idx] > classAV2.v[lt])]

    selML.idx <- selM.idx[which(beta2.v[selM.idx] < classAV2.v[lt])]

    ### find prob according to typeII distribution
    p.v <- pbeta(beta2.v[selMR.idx], em2.o$a[lt, 1], em2.o$b[lt, 1],
      lower.tail =
        FALSE
    )

    ### find corresponding quantile in type I distribution
    q.v <- qbeta(p.v, em1.o$a[lt, 1], em1.o$b[lt, 1], lower.tail = FALSE)

    nbeta2.v[selMR.idx] <- q.v



    if (doH) {
      ### if TRUE also correct type2 hemimethylated probes
      ### select H probes and include ML probes (left ML tail is not well described by a beta-distribution).
      lt <- 2

      selH.idx <- c(which(class2.v == lt), selML.idx)

      minH <- min(beta2.v[selH.idx], na.rm = TRUE)
      maxH <- max(beta2.v[selH.idx], na.rm = TRUE)
      deltaH <- maxH - minH

      #### need to do some patching
      deltaUH <- -max(beta2.v[selU.idx], na.rm = TRUE) + min(beta2.v[selH.idx],
        na.rm =
          TRUE
      )
      deltaHM <- -max(beta2.v[selH.idx], na.rm = TRUE) + min(beta2.v[selMR.idx],
        na.rm =
          TRUE
      )

      ## new maximum of H probes should be
      nmaxH <- min(nbeta2.v[selMR.idx], na.rm = TRUE) - deltaHM

      ## new minimum of H probes should be
      nminH <- max(nbeta2.v[selU.idx], na.rm = TRUE) + deltaUH

      ndeltaH <- nmaxH - nminH


      ### perform conformal transformation (shift+dilation)
      ## new_beta_H(i) = a + hf*(beta_H(i)-minH);
      hf <- ndeltaH / deltaH

      ### fix lower point first
      nbeta2.v[selH.idx] <- nminH + hf * (beta2.v[selH.idx] - minH)
    }


    ### generate final plot to check normalisation
    if (plots) {
      message("Generating final plot")

      d1.o <- density(beta1.v)

      d2.o <- density(beta2.v)

      d2n.o <- density(nbeta2.v)

      ymax <- max(d2.o$y, d1.o$y, d2n.o$y)

      plot(
        density(beta2.v),
        type = "l",
        ylim = c(0, ymax),
        xlim = c(0, 1),
        main = paste("CheckBMIQ-", sampleID, sep = "")
      )

      points(d1.o$x, d1.o$y, col = "red", type = "l")

      points(d2n.o$x, d2n.o$y, col = "blue", type = "l")

      legend(
        x = 0.5,
        y = ymax,
        legend = c("type1", "type2", "type2-BMIQ"),
        bty = "n",
        fill = c("red", "black", "blue")
      )
    }

    datM[ii, ] <- nbeta2.v
  } # end of for (ii=1 loop
  datM
}
