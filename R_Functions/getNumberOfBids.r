getNumberOfBids <- function(x){
  return(apply(x, 2, function(x) length(na.omit(x))))
}
