
# get the lps of the BidCurves
getLPOfBidCurves <- function(x)
{
  lps<-matrix(NA, ncol=dim(x)[2]/2, nrow=dim(x)[1])
  for(i in 1:(dim(x)[2]/2))
  {
    lps[,i]<-x[,2*i]
  }
  return(lps)
}
