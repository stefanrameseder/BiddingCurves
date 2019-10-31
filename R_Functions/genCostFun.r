
# cost function for generalized simulated annealing
genCostFun <- function(par, timeSeries, max, apLoss = 380, giveBackEst=FALSE){
  
  #steps = 1
  #timeSeries <- ts 
  n <- dim(timeSeries)[1] # Anzahl der Beobachtungen

  #seq(from = steps+1, to=n)
  
  #for(t in seq(from = steps+1, to=n))
  #{ 
    #timeSeries[t-1, ]
    #est[t] <- sum( timeSeries[t-1, ] * par) 
  #}
 
  est <- c(NA, timeSeries %*% par)
  
 
  estWithoutLast <- est[-(n+1)]
  

  #cbind(max, estWithoutLast, timeSeries)
  accEst <-  max >= estWithoutLast # which were accepted?
  
 
  lostMWh <-  sum(!accEst, na.rm=TRUE) * apLoss # the ones which were not accpeted times the aploss
  
  sumMaxLP <- sum(max[2:length(max)], na.rm=TRUE) # starting at the second point
  
  # lost profit = sumMax + lostMWh - sum accepted estimated LPs 
  lostProfit <-  sumMaxLP + lostMWh - sum(estWithoutLast[accEst], na.rm=TRUE)
  
  if(!giveBackEst)  return( lostProfit=lostProfit)
  else return(list(lostProfit=lostProfit, est=estWithoutLast, future=est[length(est)])) # the future one as well
}

