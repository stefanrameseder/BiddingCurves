
plot3dContour <- function(bid_data, filename, snapshot=F){
  
  lpRange <- c(0, max(apply(bid_data$price, 2, max, na.rm=TRUE)))
  zLength <- lpRange[2] - lpRange[1] + 1
  colorlut <- rainbow(zLength) # height color lookup table
  col <- colorlut[(t(bid_data$price)-lpRange[1]+1)]
  
  surface3d(x=bid_data$dates, y=t(bid_data$mws), z=t(bid_data$price), color=col, main = "test")
  grid3d(c("x", "y+", "z"), n =20)
  
  # y Axis
  yAxis = seq(from= 0, to =max(apply(bid_data$mws, 2, max, na.rm=TRUE)), by=250)
  yAxisLabels <- paste0(yAxis, " MW")
  # x Axis
  xAxis = bid_data$dates[seq(from= 1, to =length(bid_data$dates), by=12)]
  xAxisLabels <- format(xAxis, "%m-%y")
  # z Axis
  zAxis = seq(from= 0, to =max(apply(bid_data$price, 2, max, na.rm=TRUE)), by=100)
  zAxisLabels <- paste0(zAxis, " Euro/MW/w")
  
  axis3d(edge = 'y+', at = yAxis, labels = yAxisLabels, cex = 0.85)
  axis3d(edge = 'x', at = xAxis , labels = xAxisLabels, cex = 0.85)
  axis3d(edge = 'z-', at = zAxis , labels = zAxisLabels, cex = 0.85)
  title3d(main = "SRL POS Hochtarif", cex=3)
  
  if(snapshot==T){
    rgl.snapshot(paste(filename, "_bidcurves3d3.pdf"))
  }
  return()
}

#