bid_data <- data_l 

point_col	<- rgb(174/255, 167/255, 0)

curve_index = 13  
adj_xmax = max(round(bid_data$mwsc[,curve_index], -2), na.rm=TRUE)+100
xbreaks=seq(0, adj_xmax, by=200)
adj_ymax = max(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)+50
adj_ymin = 0
ybreaks=seq(adj_ymin, adj_ymax, length.out=10)

a <- ggplot(data.frame(y=bid_data$price_clean[,curve_index],x=bid_data$mwsc[,curve_index]), aes(x, y)) +       
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
      ggtitle(paste("Cap. Price SRL POS Peak in Week: ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15)) +
	  geom_vline(xintercept = 2101, col = "darkred") +
	  annotate("text", x = 1800, y = 300, label = "D(t) = 2101", col ="darkred") + 
	  annotate("text", x = 1750, y = 450, label = "Max. Price  = 441")


curve_index = 14  
adj_xmax = max(round(bid_data$mwsc[,curve_index], -2), na.rm=TRUE)+100
xbreaks=seq(0, adj_xmax, by=200)
adj_ymax = max(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)+50
adj_ymin = 0
ybreaks=seq(adj_ymin, adj_ymax, length.out=10)

b <- ggplot(data.frame(y=bid_data$price_clean[,curve_index],x=bid_data$mwsc[,curve_index]), aes(x, y)) +       
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
      ggtitle(paste("Cap. Price SRL POS Peak in Week: ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15)) +
	  geom_vline(xintercept = 2101, col = "darkred") +
	  annotate("text", x = 1800, y = 300, label = "D(t) = 2101", col ="darkred") + 
	  annotate("text", x = 1750, y = 440, label = "Max. Price  = 428")
		
curve_index = 15  
adj_xmax = max(round(bid_data$mwsc[,curve_index], -2), na.rm=TRUE)+100
xbreaks=seq(0, adj_xmax, by=200)
adj_ymax = max(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)+50
adj_ymin = 0
ybreaks=seq(adj_ymin, adj_ymax, length.out=10)

c <- ggplot(data.frame(y=bid_data$price_clean[,curve_index],x=bid_data$mwsc[,curve_index]), aes(x, y)) +       
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
      ggtitle(paste("Cap. Price SRL POS Peak in Week: ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
      theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
      theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
      theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15)) +
	  geom_vline(xintercept = 2073, col = "darkred") +
	  annotate("text", x = 1800, y = 300, label = "D(t) = 2073", col ="darkred") + 
	  annotate("text", x = 1750, y = 430, label = "Max. Price  = 437")
		

ggsave(a, file=paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_a.pdf"))
ggsave(b, file=paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_b.pdf"))
ggsave(c, file=paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_c.pdf"))

pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_ab.pdf"), width = 14)
multiplot(plotlist = list(a, b), cols=2)
dev.off()

pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_bc.pdf"), width = 14)
multiplot(plotlist = list(b, c), cols=2)
dev.off()

pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bidcurve_abc.pdf"), width = 24)
multiplot(plotlist = list(a, b, c), cols=3)
dev.off()

options(warn = 0)