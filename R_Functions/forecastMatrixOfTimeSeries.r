##############################################

getForecastMatrixOfTimeSeries <- function(timeSeries, reg=FALSE,calN=30)
{
  rolN <- dim(bidData$quantiles)[1] - calN # rolling Number
  
  # empty matrix for one-step forecasts
  forecastMatrix <- matrix(NA, nrow=dim(timeSeries)[1], ncol=dim(timeSeries)[2]*3)
  colnames(forecastMatrix) <- c(t(matrix(c(paste0("lo ", colnames(timeSeries)),
                                           paste0(colnames(timeSeries)),
                                           paste0("hi ", colnames(timeSeries))), dim(timeSeries)[2])))
  
  if(isTRUE(!reg)) ### without regressors
  {
    for(i in 1:rolN)
    {
      print(i)
      steplength <- i + calN -1 
      for(name in colnames(timeSeries))
      {
        fc <- forecast( auto.arima( timeSeries[ (i-1):steplength , name]) , h=1)
        forecastMatrix[ steplength+1 , paste0("lo ", name)] <- fc$lower[1]
        forecastMatrix[ steplength+1 , paste0(name)] <- fc$mean
        forecastMatrix[ steplength+1 , paste0("hi ", name)] <- fc$upper[1]
      }
      
    }  
  }
  else if(is.matrix(reg)) # with regressors
  {
    for(i in 1:rolN)
    {
      print(i)
      steplength <- i + calN -1 
      for(name in colnames(timeSeries))
      {
        if(all(apply(reg, 2, sum)>0)) # in the case where christmas can be 0
        {
          model <- auto.arima( timeSeries[ i+1:steplength , name] , max.p=2, seasonal=FALSE, max.d=0, max.D=0, xreg=reg[ (i-1):(steplength-1) , ])
          fc <- forecast(model, h=1, xreg=reg[ i:steplength , ])
          ?auto.arima
          forecastMatrix[ steplength+1 , paste0("lo ", name)] <- fc$lower[1,1]
          forecastMatrix[ steplength+1 , paste0(name)] <- fc$mean[1]
          forecastMatrix[ steplength+1 , paste0("hi ", name)] <- fc$upper[1,1]
        }
        else
        {
          reg <- reg[, -which(apply(reg, 2, sum) == 0)] # lösche die konstanten Einträge
          model <- auto.arima( timeSeries[ i+1:steplength , name] , max.p=2, seasonal=FALSE, max.d=0, max.D=0, xreg=reg[ (i-1):(steplength-1) , ])
          fc <- forecast(model, h=1, xreg=reg[ i:steplength , ])
          
          forecastMatrix[ steplength+1 , paste0("lo ", name)] <- fc$lower[1,1]
          forecastMatrix[ steplength+1 , paste0(name)] <- fc$mean[1]
          forecastMatrix[ steplength+1 , paste0("hi ", name)] <- fc$upper[1,1]
        }  
      }
    }
  }
  return(forecastMatrix)
}


