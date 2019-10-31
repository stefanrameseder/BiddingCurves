


GetRho <- function(y, t, optns, mu, obsGrid, fittedCov, lambda, phi, sigma2) {
  
  optnsTmp <- optns
  optnsTmp$verbose <- FALSE 
  for (j in 1:2) {
    yhat <- GetCEScores(y, t, optnsTmp, mu, obsGrid, fittedCov, lambda, phi, sigma2)[3, ] 
    sigma2 <- mean(mapply(function(a, b) mean((a - b)^2, na.rm=TRUE), yhat, y), na.rm=TRUE)
  }
    
  R <- sqrt((trapzRcpp(obsGrid, mu ^ 2) + sum(lambda)) / diff(range(obsGrid)))
  a1 <- 0.01; a2 <- 0.22
  etaCand <- seq(a1, a2, length.out=50)
  rhoCand <- etaCand * R
  rhoCand <- rhoCand[rhoCand > sigma2]
  rhoCand <- c(sigma2, rhoCand)
  
  leaveOutInd <- RandTime(t, isRandom=FALSE)

  cvScores <- sapply(rhoCand, cvRho, leaveOutInd=leaveOutInd, y=y, t=t, optns=optns, mu=mu, obsGrid=obsGrid, fittedCov=fittedCov, lambda=lambda, phi=phi)

  return(rhoCand[which.min(cvScores)])
}

