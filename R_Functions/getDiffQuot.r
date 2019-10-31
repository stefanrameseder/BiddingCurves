##############################################
# Numerical Differentiation
##############################################
##############################################

# for a quick check:
#x <- seq(0,2*pi, by = 0.01)
#y <- sin(x)

#quotients <- difference_quotients(y = y, x = x)

#plot(x, cos(x), type="l", lwd=2) # analytical derivative of sin(x) is cos(x)
#lines(x, quotients$back_quot, type="l", col="red")
#lines(x, quotients$for_quot, type="l", col="green")
#lines(x, quotients$cent_quot, type="l", col="blue")
getDiffQuot 	<- function(y , x){
  # calculates different difference quotients
  # Input:
  # two vectors y and x, numerator y and denominator x
  # Ouput:
  # a vector of same length as y with the difference quotients
  # forward quotient y'_t = y_t+1 - y_t / x_t+1 - x_t
  # backward quotient y'_t+1= y_t+1 - y_t / x_t+1 - x_t
  # centered difference quotient y_t+1 - 2 * y_t +y_t-1 / (x_t+1 - x_t)*(x_t - x_t-1)
  
  # check for same length
  if(length(y) != length(x) ){
    stop("length of y is not equal to length of x; cut y down")
    y <- y[1:length(x)]
  } 
  
  quot <- ( y[-1] - y[-length(y)] ) /  ( x[-1] - x[-length(y)] )
  for_quot <- c( quot, quot[length(quot)] ) 
  back_quot <- c( quot[length(quot)] , quot ) 
  
  # y't = (y_t+1 - y_t-1 )/ (x_t+1 - x_t + x_t - x_t-2) = (y_t+1 - y_t-1 )/(x_t+1 - x_t-2); t = 1, ..., T-1
  centered_quot <- ( y[-(1:2)] - y[-((length(y)-1) : length(y))] ) / ( x[-(1:2)] -  x[-((length(y)-1) : length(y))]) 
  
  cent_quot <- c ( quot[1], centered_quot , quot[length(quot)] )
  
  return( list(for_quot = for_quot, back_quot = back_quot, cent_quot = cent_quot) )
}


##############################################
# Monotone P-Splines
##############################################
##############################################

# monotPSpline <- function( x, y, eval_x = x, df = NULL , lambda = NULL, knots = NULL, increasing = TRUE, order = 4) {
  # Input:
  # - x, y: Values in Domain and Image
  # - eval_x: x Values where function/derivative should be calculated
  # - df: degrees of freedom
  # - lambda=1: comes from penalized log likelihood aka penalty parameter: L = (y - f)' W (y - f) + lambda c' S c
  # - knots: knot sequence (including the repeated boundary knots)
  # - increasing: defines the restriction matrix for the QP accordingly
  # Output:
  # - coefs: list with homogenous and inhomogenous solution
  # - B: Evaluated Basis
  # -"unconstrained.curve"  = fu.unconst,
  # -"constrained.curve"  = fu.constr,
  # -"curve.firstderiv.u" = first.deriv.unconst,
  # -"curve.firstderiv.c"  = first.deriv.constr,
  # - knots: knot sequence
  # - df: df

  # If df provided, presmooth using smooth.spline
  # if (!is.null(df)){
    # smspl      <- smooth.spline(x, y, df = df)
  # } else {
    # smspl      <- smooth.spline(x, y, spar = lambda) # cv = FALSE -> GCV
	# fda <- with(data.frame(x=x, y=y), smooth.basisPar(argvals = x, y = y, lambda = 0.5))
	# plot(fda$fd, xlab="age", ylab="height (cm)", main="Girls in Berkeley Growth Study" )
	# plot(deriv(fda$fd), xlab="age", ylab="growth rate (cm / year)", main="Girls in Berkeley Growth Study" )
	# dev.off()
  # }
  # ?smooth.spline
  # knots      <- smspl$fit$min + smspl$fit$knot*smspl$fit$range # multiple knots possible
  
  # If lambda is not provided, get it from presmoothing
  # if (is.null(lambda)) lambda <- smspl$lambda
  
  # dim      <- smspl$fit$nk # number of different knots
  
  # df       <- smspl$df
  
  ##########################################################################################
  # Constraints
  # Define Matrix for Constraints; cf. Mayer Article
  # C       <- matrix(0,dim,dim)
  # diag(C)     <- 1     # diagonal
  # diag(C[1:dim-1,2:dim])  <- -1 # upper diagonal
  
  # if (increasing==FALSE) C  <- -C # then it s decreasing
  
  # Omega
  # Omega      <- GramMatrix(deriv(deriv(SplineBasis(knots, order = order)))) # Get Omega Matrix = 2nd derivative of BSplines
  # Basis
  # B       <- evaluate(SplineBasis( knots, order = order), x)
  # Matrices for Optimization via Quadprog
  # Dmat      <- t(B)%*%B + lambda * Omega
  # Amat      <- C 
  # dvec      <- as.numeric(t(y)%*%B)
  # Use solve.QP from quadprog to solve for min over all b: (-dvec^T b + 1/2 b^T Dmat b) w.r.t. Amat^T b >= b_0.
  # solve      <- solve.QP(Dmat, dvec, Amat)
  
   # B       <- evaluate(SplineBasis( knots, order = order), eval_x)

  
  ##########################################################################################
  # Collect Listentries for Return
  # Optimization Solutions
  # coefs      <- list( solve$solution, solve$unconstrained.solution)
  # names(coefs)    <- c("constrained", "unconstrained")
  
  # Functions to use
  # fu.constr     <- Vectorize(function(x) return(evaluate(SplineBasis(knots,order = order),eval_x) %*% solve$solution ))
  # fu.unconst     <- Vectorize(function(x) return(evaluate(SplineBasis(knots,order = order),eval_x) %*% solve$unconstrained.solution))
  # first.deriv.unconst  <- Vectorize(function(x) return(evaluate(deriv(SplineBasis(knots,order = order)),eval_x)  %*% solve$unconstrained.solution))
  # first.deriv.constr   <- Vectorize(function(x) return(evaluate(deriv(SplineBasis(knots,order = order)),eval_x)  %*%  solve$solution))
  
  # predicted values
  # predict.constr    <- evaluate(SplineBasis(knots,order = order),eval_x) %*% solve$solution
  
  # predict.firstDeriv   <- evaluate(deriv(SplineBasis(knots,order = order)),eval_x) %*% solve$solution
  
  
  
  # output      <- list("solution"     			= coefs,
                      # "eval.basis"    			= B,
                      # "unconstrained.curve" 	= fu.unconst,
                      # "constrained.curve"  		= fu.constr,
                      # "curve.firstderiv.u" 		= first.deriv.unconst,
                      # "curve.firstderiv.c"  	= first.deriv.constr,
                      # "predict.val.c" 			= predict.constr,
                      # "predict.firstDeriv.c" 	= predict.firstDeriv,
                      # "knots"     				= knots,
                      # "df"     					= df)
  # return(output)
# }


# x <- c( 10 ,  20 ,  25 ,  50 ,  75,   90 , 100 , 110,  125,  135 , 215 , 225 , 305 , 315,  395 , 454,  464 , 544  ,554 , 564,  644,  654 , 764,  774,
        # 784,  794,  804 , 814 , 894, 1009, 1019 ,1029, 1039 ,1049, 1059, 1069 ,1167, 1227 ,1237, 1247 ,1257, 1267 ,1277, 1397, 1407, 1417, 1497 ,1507,
        # 1567 ,1577 ,1682 ,1742 ,1802 ,1862 ,1902, 1962 ,2022, 2032 ,2062 ,2167)

# y <- c(450.0,  460.0,  476.0 , 500.0 , 525.0 , 546.0 , 550.0,  569.8 , 576.0  ,600.0,  629.4 , 650.0 , 669.8 , 700.0,  729.7 , 748.0,  750.0,
       # 758.9,  767.0 , 767.0,  768.4,  777.0 , 780.0 , 787.0,  797.0 , 800.0 , 807.0 , 817.0 , 819.4,  825.0 , 827.0 , 837.0 , 847.0,  850.0,
       # 857.0,  867.0 , 874.0 , 876.7 , 877.0 , 887.0 , 897.0 , 900.0 , 907.0 , 912.0 , 917.0 , 927.0 , 930.8 , 937.0 , 948.0 , 950.0, 953.0,
       # 957.0 , 969.0 , 979.0 , 985.0 , 989.0 ,1000.0, 1000.0, 1010.0 ,1037.0)

# length(y)
	   
# plot(x, y, type ="l")
# spl <- monotPSpline( x = x,y = y, eval_x = seq(0,2000, length.out=50), lambda = 1)
# lines(x, spl$eval.basis%*% spl$solution$constrained, col = "red")
# plot(seq(0,2000, length.out=50), spl$predict.firstDeriv.c, type="l")
# abline(h=0, col = "black")
# deriv <- spl$curve.firstderiv.c
# curve(deriv, add=TRUE) 


# y <- y_l[[ i ]]; x <- u_l[[ i ]]

# class(deriv)

# deriv(1)

