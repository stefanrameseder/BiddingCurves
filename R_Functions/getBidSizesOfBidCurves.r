# get the mw sizes of each bid of the BidCurves
getBidSizesOfBidCurves <- function(x)
{
  # apply to 0 added mws the diff function onto rows
  return( apply(rbind(0, x) , 2, diff) )
}
