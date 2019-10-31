##############################################
##############################################
# Data Preparation
##############################################
##############################################

# get the mw positions of the BidCurves
getMWDataOfBidCurves <- function(x){
  mws<-matrix(NA, ncol=dim(x)[2]/2, nrow=dim(x)[1])
  for(i in 1:(dim(x)[2]/2))
  {
    mws[,i]<-x[,2*(i-1)+1]
  }
  return(mws)
}
