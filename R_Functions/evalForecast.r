
evalForecast <- function(forecastMatrix, bidDataQuantiles, apLoss, RollingForecast=TRUE)
{
  calN<-sum(is.na(forecastMatrix[,1]))
  rolN<-sum(!is.na(forecastMatrix[,1]))
  

  
  # Probability Stuff
  string<-paste0("Descriptives of Forecast\n-Number of Obs: ",calN+rolN, " with Date Range: ", min(bidDataQuantiles$dates), " - ", max(bidDataQuantiles$dates),
                 "\n-Calibration Obs.: ",calN, 
                 "\n-Forecast Obs.: ",rolN, " with Forecast Range: ", bidDataQuantiles$dates[calN+1], " - ", max(bidDataQuantiles$dates),
                 "\n-Number of Time Series: ", dim(forecastMatrix)[2]/3,
                 "\n\nProbability Evaluation (<=LP)")
  
  
  
  
  
  accTimes <- apply(forecastMatrix, 2, function(X) sum(na.omit(X >= bidDataQuantiles$max)))
  accProbs <- apply(forecastMatrix, 2, function(X) round(sum(na.omit(X <= bidDataQuantiles$max))/rolN, 2)*100)
  
  for(k in seq(1,length(accProbs),3) )
  {
    string <- paste0(string, "\n-", names(accProbs)[k+1], "\t (", accProbs[k], "%)  ", accProbs[k+1], "% (", accTimes[k+1], "/", rolN,")  (", accProbs[k+2], "%)")
  }
  
  # Money Stuff
  string<-paste0(string, "\n\nMonetary Evaluation in Forecast Period (cf. Min= ", round(sum(bidDataQuantiles$min[(calN+1):(calN+rolN)]), 0) ,
                 "\u20ac/MW, GAvg= ", round(sum(bidDataQuantiles$gavg[(calN+1):(calN+rolN)]), 0) ,
                 "\u20ac/MW, Max= ", round(sum(bidDataQuantiles$max[(calN+1):(calN+rolN)]), 0) ,"\u20ac/MW)")
  
  
  accMoney <- round(apply(forecastMatrix[(calN+1):(calN+rolN),], 2, function(X) sum(X[X<=bidDataQuantiles$max[(calN+1):(calN+rolN)]])),0)
  
  for(k in seq(1,length(accProbs),3) )
  {
    string <- paste0(string, "\n-", names(accMoney)[k+1], "\t (", accMoney[k], "\u20ac)  ", 
                     accMoney[k+1], "\u20ac [", round(accMoney[k+1]/sum(bidDataQuantiles$gavg[(calN+1):(calN+rolN)])*100, 0)  ,
                     "% of GAvg] (", accMoney[k+2], "\u20ac); with loss: ",accMoney[k+1]-accTimes[k+1]*apLoss ,"\u20ac (", apLoss,"/w)")
  }
  
  cat(string)
  # costs of our time series is lost profit and lost profit = 
  return(list(Output=string, costs=sum(na.omit(bidDataQuantiles$max[calN+1:length(bidDataQuantiles$max)]))-accMoney[2]+accTimes[2]*apLoss
         , percent=round(accMoney[k+1]/sum(bidDataQuantiles$gavg[(calN+1):(calN+rolN)])*100, 0)))
}

