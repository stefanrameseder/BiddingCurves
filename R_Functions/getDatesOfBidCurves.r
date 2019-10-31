# get the dates of the BidCurves
getDatesOfBidCurves <- function(x)
{
  dates<-c(rep(NA, times=dim(x)[2]/2))
  for(i in 1:(dim(x)[2]/2))
  {
    dates[i]<-format(as.Date(unlist(strsplit(colnames(x)[2*i], "R"))[2], "%d.%m.%Y"),format="%Y-%m-%d")
  }
  return(as.Date(dates))
}