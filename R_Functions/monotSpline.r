## The function depends on:  'quadprog', 'orthogonalsplinebasis'

# Demo of the monotSpline Function
# x <- seq(0,1, length.out = 100)
# y <- cumsum(abs(rnorm(100)))
# plot(x,y, type = "l", lwd = 3)
# lines( x = x, y = monotSpline(cbind(x,y))$constrained.eval, col = "red", lty = 2)
# lines( x = x, y = monotSpline(cbind(x,y))$unconstrained.eval, col = "green", lty = 2)


monotSpline <- function(curvedata, deriv = 0, lambda_opt = TRUE, increasing = TRUE, 
                        trace = FALSE, eval_x = curvedata[, 1]){
  ### DATE: 27.03.2016
  
  ### SUMMARY:
  ## Given datapoints x_i, y_i, fits monotone penalized cubic spline with optimal lambda
  ## according to Meyer 2010 - Constrained Penalized Splines
  
  ### INPUT:
  ## - curvedata: matrix of length T with entries x_i,y_i,i=1,...,T
  ## - deriv = 0: derivative of splines
  ## - increasing: logical, wheather increasing or decreasing spline (optional)
  ## - lambda_opt = TRUE; should optimal lambda be chosen or do you want to choose it
  ## - trace = false; do you want to trace optimization 
  ## - eval_x = x_i: evaluation grid where splines should be evaluated

  ### RETURNS:
  ## list of length 5 with entries:
  ## - constrained.coefs: constrained coefficients for cubic B-Splines
  ## - unconstrained.coefs: unconstrained coefficients for cubic B-Splines
  ## - x.basis: Basis evaluated at x
  ## - eval_x.basis: Basis evaluated at eval_x
  ## - unconstrained.curve: predicted unconstrained values at eval_x
  ## - constrained.curve: predicted constrained values at eval_x
  ## - knots: knot choice sequence
  ## - df: edf = effective degrees of freedom
  ## - lambda: optimal lambda or chosen lambda for penalizing second derivative

  ## NOTE:
  ## depends:  'coneproj', 'orthogonalsplinebasis', 'MASS'
  
  ## determine number of knots, borrowed from smooth.spline:
  tol           	<- 1e-6
  x              	<- curvedata[,1]
  y              	<- curvedata[,2]
  xx           		<- round((x - mean(x))/tol)
  nd           		<- !duplicated(xx)
  ux           		<- sort(x[nd])
  uxx        		<- sort(xx[nd])
  nx           		<- length(ux)
  r.ux        		<- ux[nx] - ux[1L]
  xbar       		<- (ux - ux[1L])/r.ux
  nx1         		<- length(x)  -  sum(duplicated( round((x - mean(x)) / tol)))
  
  ## interior knots
  nknots   			<- round(.nknots.smspl(nx)/2)
  knot        		<- c(rep(xbar[1], 3), xbar[seq.int(1,nx, length.out = nknots)], rep(xbar[nx], 3))*r.ux
  
  ## number of basis functions
  dim         		<- nknots + 2 # cubic splines only
  
  ## constrained matrix generation
  cMat          	<- matrix(0,dim-1,dim)
  diag(cMat)    	<- -1
  diag(cMat[1:dim-1,2:dim])   <- 1
  if (increasing==FALSE) cMat <- - cMat # then its decreasing
  
  ## Spline Basis evaluated at x and eval_x
  B             	<- evaluate(SplineBasis(knot,order=4), as.vector(x))
  B_eval_x      	<- evaluate(SplineBasis(knot,order=4), as.vector(eval_x))
  
  
  ## Omega: Matrix of evaluated second derivatives since second derivatives will be penalized
  Omega         	<- GramMatrix(deriv(SplineBasis(knot,order=4),2))
  k             	<- dim - 1
  
  ## if true, find optimal lambda; optimization is computational intensive!!!
  if(lambda_opt == TRUE) {
    
    ## Cross Validation function for optimal lambda
    CV              <- function(lambda){
      # if trace == TRUE optimization will be traced by printing
      if(trace == TRUE) print(lambda)
      resI <- function(i){
        ## i <- 1
        index    <- rep(TRUE,length(x))
        index[i] <- FALSE 
        fmat     <- t(B[index,]) %*% B[index,] + lambda * Omega
        L        <- t( chol(fmat))
        Linv     <- solve(L)
        A        <- cMat %*% t(Linv)
        z        <- as.vector( Linv %*% t( B[index,]) %*% y[index])
        cproj    <- coneA(z,A)
        return( B[!index,] %*% t(Linv) %*% cproj$thetahat - y[!index] )
      }
      resids <- as.numeric( lapply(1:length(x),resI) )
      return(sum(resids)^2)
    }
    lambda  <- optim(0,CV,method="L-BFGS-B",lower=0,upper=Inf)$par
  } else {
    lambda <- lambda_opt
  }
  
  ## calculate fit with optimal lambda
  fmat          <- t(B) %*% B + lambda * Omega 
  
  ## Choleski, notation according to Meyer (2012)
  L             <- t( chol( fmat ))
  A             <- cMat %*% solve(t(L))
  z             <- as.vector(solve(L) %*% t(B) %*% y)
  
  ## Cone projection, requires package croneproj
  delta         <- t(crossprod(A, solve(tcrossprod(A))))
  cb            <- coneB(z,delta)
  
  eJ            <- Null(t(A))
  FaceJ         <- cb$coef!=0 
  DeltaJ        <- cbind(t(delta)[,FaceJ],eJ)
  
  ## DeltaJ (Matrix of edges and nullspace-vectors)
  ## for computing the edf
  yMat          <- solve( t(L)) %*% DeltaJ %*% solve( t(DeltaJ) %*% DeltaJ) %*%
                    t(DeltaJ) %*% solve(L) %*% t(B)
  PJ            <- B %*% yMat
  coefs_c       <- yMat %*% y
  
  ## test: sum(abs(PJ%*%y-B %*% coefs))
  df            <- sum(diag(PJ))
  
  ## unconstrained solution, same lambda
  coefs_uc      <-  solve( t(B) %*% B + lambda * Omega) %*% t(B) %*% y
  
  ## Return of constrained and unconstrained function/derivative values at eval_x
  if(deriv == 0){
    y_constr      <- evaluate(SplineBasis(knot,order=4), eval_x) %*% coefs_c
    y_unconstr    <- evaluate(SplineBasis(knot,order=4), eval_x) %*% coefs_uc
  } else {
    y_constr      <- evaluate(deriv(SplineBasis(knot,order=4),  deriv), eval_x) %*% coefs_c
    y_unconstr    <- evaluate(deriv(SplineBasis(knot,order=4), deriv ), eval_x) %*% coefs_uc
  }
  ## Return of constrained and unconstrained functions and arbitrary derivates
  f_constr      <- function(x, deriv = 0 ){ 
      if(deriv == 0){
          evalPoints        <- evaluate(SplineBasis(knot,order=4), x) %*% coefs_c
        } else if (deriv > 0) {
          evalPoints        <- evaluate(deriv(SplineBasis(knot,order=4), deriv ), x) %*% coefs_c
        }
      return(as.vector(evalPoints)) 
      }
  
  f_unconstr    <- function(x, deriv = 0 ){ 
    if(deriv == 0){
      evalPoints        <- evaluate(SplineBasis(knot,order=4), x) %*% coefs_uc
    } else if (deriv > 0) {
      evalPoints        <- evaluate(deriv(SplineBasis(knot,order=4), deriv ), x) %*% coefs_uc
    }
    return(as.vector(evalPoints)) 
  }    # f_constr(com_dom_grid_v, 1)

  # Collect return
  ret           <- list(coefs_c, coefs_uc, 
                        B, B_eval_x, 
                        y_constr, y_unconstr, 
                        f_constr, f_unconstr,
                        knot, df, lambda)
  names(ret)    <- c("constrained.coefs", "unconstrained.coefs", 
                     "x.basis", "eval_x.basis",
                     "constrained.eval",  "unconstrained.eval",
                     "constrained.curve", "unconstrained.curve",
                     "knots","df","lambda")
  return(ret)
}

