## Function Generator as a sum of eigenvalues and eigenfunctions

genFunctionByKL	<- function(x = SP.vec, lambda=lambda.vec, K = length(lambda.vec), efun.select = c(1,2,3)){
  ## Input:
  # - x: Discrete Points on which functional variable lives on
  # - lambda: Eigenvalues 
  # - K: Number of Eigenfunctions or Dimnesion of True Space
  # - efun.select: whhich eigenfunction should be selected
  ## Output:
  # - A vector of random function values observed at x with given linear combinations of score_k (with variances lambda_k^2) * eigenfunctions
  
  # Check for 3
  if(length(lambda.vec)!=3){stop}
    scores.all <- numeric(K)    
    for(k in 1:K){
        scores.all[k] <- rnorm(1, 0, lambda[k])
    }
    scores <- c(0,0,0)
    scores[efun.select] <- scores.all[efun.select]
    
	Fct <- e.fun1(x) * scores[1] +
           e.fun2(x) * scores[2] +
           e.fun3(x) * scores[3]
    return(Fct)
}

