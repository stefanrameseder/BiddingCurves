
# sigma2* = max(rho, sigma2) = rho
# Get the CV score for a given rho. Correspond to getScores2.m
cvRho <- function(rho, leaveOutInd, y, t, optns, mu, obsGrid, fittedCov, lambda, phi) {

  Sigma_Y <- fittedCov + diag(rho, nrow(phi))

  MuPhiSig <- GetMuPhiSig(t, obsGrid, mu, phi, Sigma_Y)

  yhat <- mapply(function(yVec, muphisig, ind) 
         GetIndCEScores(yVec, muphisig$muVec, lambda, muphisig$phiMat,
                        muphisig$Sigma_Yi, newyInd=ind)$fittedY, 
                 y, MuPhiSig, leaveOutInd) 
  
  yobs <- mapply(`[`, y, leaveOutInd)

  return(sum((na.omit(unlist(yobs)) - unlist(yhat))^2, na.rm=TRUE))
}

