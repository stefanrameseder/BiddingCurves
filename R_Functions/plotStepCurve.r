
plotStepCurve <- function(bid_data, curve_index, smooth=F, smooth_data=NULL, point_col=rgb(174/255, 167/255, 0)){
  # curve_index = 2 scalar; number of observation of step curve
  # point_col = lärchennadelgrün
  # bid_data=data_l
  # smooth=TRUE
  # smooth_data=smooth_data
  # round(2250, -2)
  # curve_index= i 
  
  adj_xmax = max(round(bid_data$mwsc[,curve_index], -2), na.rm=TRUE)+100
  xbreaks=seq(0, adj_xmax, by=200)
  
  adj_ymax = max(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)+50
  adj_ymin = min(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)-100
  ybreaks=seq(adj_ymin, adj_ymax, length.out=10)
  if(smooth==FALSE){
    step_curve <- ggplot(data.frame(y=bid_data$price_clean[,curve_index],x=bid_data$mwsc[,curve_index]), aes(x, y)) +       
      geom_point(colour=point_col , size=3) + 
      geom_step() +
      theme_bw() + 
      scale_x_continuous(breaks = xbreaks, 
                         labels = xbreaks, 
                         limits = c(0,adj_xmax), expand = c(0,0)) +
      scale_y_continuous(breaks = ybreaks, 
                         labels = paste0(round(ybreaks, -1), " Euro / MW"), limits=range(ybreaks)) +
      ylab(expression(paste(y[it], ': Capacity Price in Euro/MW/w'))) + # 
      xlab(expression(paste(x[it], ': Cumulated Capacity'))) +
      ggtitle(paste("Cap. Price SRL POS Peak ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15))
  }
  if(smooth==TRUE && !is.null(smooth_data)){
    step_curve <- ggplot(data.frame(y=bid_data$price_clean[,curve_index],
                                    x=bid_data$mwsc[,curve_index]), aes(x, y)) +       
      geom_point(colour="black" , size=1) + 
      geom_step() +
      geom_line(data = smooth_data , colour = point_col, size=1.2) +
      theme_bw() + 
      scale_x_continuous(breaks = xbreaks, 
                         labels = xbreaks, 
                         limits = c(0,adj_xmax), expand = c(0,0)) +
      scale_y_continuous(breaks = ybreaks, 
                         labels = paste0(round(ybreaks, -1), " Euro / MW"), limits=range(ybreaks)) +
      ylab(expression(paste(y[it], ': Capacity Price in Euro/MW/w'))) + # 
      xlab(expression(paste(x[it], ': Cumulated Capacity'))) +
      ggtitle(paste("Cap. Price SRL POS Peak ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15))
    
  }
  return(step_curve)
}

