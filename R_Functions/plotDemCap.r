# Demanded capacity with plot
plotDemCap <- function(bid_data){
  plot <- plot(stepfun(bid_data$dates_mw,c(bid_data$demCap$SRL_POS_mw[1],bid_data$demCap$SRL_POS_mw)), ylim=c(0, 3500),
       main = "Capacity Demand by the TSO's",
       ylab="Capacity Demand in MW", xlab="Dates (Quarters and Christmas)",
       col="darkblue", lwd=2, xaxt = "n")
  lines(stepfun(bid_data$dates_mw,c(bid_data$demCap$SRL_NEG_mw[1],bid_data$demCap$SRL_NEG_mw)), col="darkred", lwd=2)
  lines(stepfun(bid_data$dates_mw,c(bid_data$demCap$MRL_NEG_mw[1],bid_data$demCap$MRL_NEG_mw)), col="red", lwd=1)
  lines(stepfun(bid_data$dates_mw,c(bid_data$demCap$MRL_POS_mw[1],bid_data$demCap$MRL_POS_mw)), col="blue", lwd=1)
  lines(stepfun(bid_data$dates_mw,c(bid_data$demCap$PRL_mw[1],bid_data$demCap$PRL_mw)), col="green", lwd=2.5)
  axis.Date(1, at = bid_data$dates_mw, format= "%Y-%m-%d")
  legend("bottom", legend=c("PRL", "SRL POS", "SRL NEG", "MRL POS", "MRL NEG"),
         col=c("green", "darkblue", "darkred", "blue", "red"), lwd=c(2.5,2, 2, 1, 1), lty=1)
  return(plot)
}
