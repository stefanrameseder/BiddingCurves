# Calculating xTicks
##############################
# this function calculates for the LPGraphs nice xTicks
getXTicks <- function(data, Number_Xticks=12){
# Former: Get_xTicks
  if(length(data)>10)
  {
    Step_Length<-floor(length(data)/(Number_Xticks +1))
    # Matrix consisting of corresponding numbers and labels
    Xticks <- matrix(NA, nrow= (Number_Xticks +1), ncol=2)
    for(i in 1: (Number_Xticks+1))
    {
      Xticks[i,1] <- 1 + (i-1)*Step_Length # corresponding number
      Xticks[i,2] <- format(as.Date(data[1 + (i-1)*Step_Length]), '%d.%m.%Y') 
    }
    
  }
return(Xticks)
}
