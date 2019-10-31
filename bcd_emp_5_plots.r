##########################################################################################
## No of Participants					     											##
srl_bidders <- c(12,	17,	16,	15,	17,	22,	23,	24,	24,	24,	19,	23,	23,	24,	21)
dates_excel       <- c("01.02.2012",	"01.04.2012",	"01.06.2012",	"01.11.2012",
                       "01.06.2013",	"01.01.2014", "01.03.2014",	"19.11.2014",	
                       "10.12.2014",	"17.03.2015",	"01.07.2015",	"21.12.2015",	
                        "03.03.2016","09.05.2016",	"03.03.2017")

dates_nice          <- as.Date(format(as.Date(dates_excel, format = "%d.%m.%Y"), format = "%Y-%m-%d"))

df <- data.frame(Date = dates_nice, Participants = srl_bidders)

library(ggplot2)
theme_set(theme_minimal())


plot <- ggplot(data = df, aes(x = Date, y = Participants, group = 1))+ 
    geom_line(color = "#00AFBB", size = 1) + 
    theme(text = element_text(size=10), axis.title.x = element_blank()) + 
    ylab("Prequalified participants") +   
    scale_x_date(labels = date_format("%m-%Y"), breaks = pretty_breaks(8)) +
    ylim(0, 30) +
	geom_hline(yintercept=0, color = "black", size=0.4)

ggsave(filename = "C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\bidders.pdf",
       plot = plot, width = 10, height = 6, units = "cm")

##########################################################################################
## Number of Bids 						     											##
library(ggplot2)
library(reshape2)

df2 <- data.frame(Date = dates, sapply(PMHTNT, function(pmhtnt) biddata[[pmhtnt]]$nBids) ) 
meltdf <- melt(df2,id="Date")

names(meltdf) <- c("Date", "Reserve", "Bids")
head(meltdf)
theme_set(theme_minimal())
plot <- ggplot(meltdf,aes(x=Date, y=Bids, colour=Reserve, group=Reserve)) + 
	geom_line(size = 0.8) + 
	theme(text = element_text(size=10), axis.title.x = element_blank()) + 
	scale_x_date(labels = date_format("%m-%Y"), breaks = pretty_breaks(8)) + 
	scale_color_manual(values = c("darkblue", "blue", "darkred", "red"))  +  
	ylim(0,180) +
	geom_hline(yintercept=0, color = "black", size=0.4)
	
	
ggsave(filename = "/Users/stefanrameseder/Google Drive/BCD_fin/bids.pdf",
       plot = plot, width = 12, height = 6, units = "cm")
	
	
ggsave(filename = "C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\bids.pdf",
       plot = plot, width = 14, height = 7, units = "cm")
	   
##########################################################################################
## Demanded capacity 						     											##
library(ggplot2)
library(reshape2)

str(biddata[[pmhtnt]], max = 1)

df2 <- data.frame(Date = dates, sapply(c("POS_HT", "NEG_HT"), function(pmhtnt) biddata[[pmhtnt]]$maxMW) ) 
names(df2) <- c("Date", "POS", "NEG")
meltdf <- melt(df2,id="Date")



names(meltdf) <- c("Date", "Reserve", "Demand")
head(meltdf)
theme_set(theme_minimal())
plot <- ggplot(meltdf,aes(x=Date, y=Demand, colour=Reserve, group=Reserve)) + 
	geom_hline(yintercept = min(biddata[[pmhtnt]]$maxMW), col = "darkgreen", size=1.2)+ 
	geom_vline(xintercept = dates[which(biddata[[pmhtnt]]$quantiles$christmas==1)], col = "darkgrey", size=1)  +
	geom_line(size = 0.8) + 
	theme(text = element_text(size=12), axis.title.x = element_blank()) + 
	scale_x_date(labels = date_format("%m-%Y"), breaks = pretty_breaks(8)) + 
	scale_color_manual(values = c("darkblue", "darkred"))  +  
	ylim(0,2600) +
	geom_hline(yintercept=0, color = "black", size=0.4) +
	scale_y_continuous(breaks = seq(0, 2500, length.out = 6), labels = paste0(seq(0, 2500, length.out = 6), " MW")) 
	
	
ggsave(filename = "C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\mwdemand.pdf", plot = plot, width = 20, height =12, units = "cm")
	
##########################################################################################
# Smoothing and discrete 
#pdf(paste0("bcd paper/graphs/", format(Sys.time(), format="%Y%m%d"), "example_curves",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
bid_data 		<- biddata[[pmhtnt]] 

curve_index 	<- 35 # 13 -16; 15 demand = demCap[2] which(dates == "2014-09-08")
point_col		<- "darkgreen"
adj_xmax 		<- max(round(bid_data$mwsc[,curve_index], -2), na.rm=TRUE)+100
xbreaks 		<- seq(0, adj_xmax, by=200)
  
adj_ymax 		<- max(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)+50
adj_ymin 		<- min(round(bid_data$price_clean[,curve_index], -2), na.rm=TRUE)-100
ybreaks 		<- seq(adj_ymin, adj_ymax, length.out=10)
ybreaks 		<- seq(200, 1000, length.out=9)


dates[curve_index]
bid_data$maxMW[curve_index]

# str(bidcurves_smoothed[[pmhtnt]][[curve_index]]$constrained.curve(md_dis), max =2)

smoothed_curve <- data.frame(x= md_dis, y = bidcurves_smoothed[[pmhtnt]][[curve_index]]$constrained.curve(md_dis))

pdf("C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\smoothedCurve.pdf", height = 7.5, width = 10)
(step_curve 		<- ggplot(data.frame(y=bid_data$price_clean[,curve_index],x=bid_data$mwsc[,curve_index]), aes(x, y, color = "Curves")) +       
					geom_vline(xintercept = bid_data$maxMW[curve_index], col = "blue", size=1.2, alpha = 0.3, aes(colour="Demand")) +
					geom_line(data = smoothed_curve, aes(colour="Smoothed"), color="red", size=4, alpha = 0.3)+
					geom_step(size = 2, colour=point_col, aes(colour="Discrete"), alpha = 0.3) +
					theme_bw() + 
					geom_point(colour="black" , size=2, aes(colour="Discrete")) + 
					theme(panel.grid.major = element_line(colour = "grey"), axis.text=element_text(size=12, face="bold")) + 
					scale_x_continuous(breaks = xbreaks[-length(xbreaks)], labels = xbreaks[-length(xbreaks)],  limits = c(0,adj_xmax), expand = c(0,0)) +
					scale_y_continuous(breaks = ybreaks, labels = paste0(round(ybreaks, -1), " Euro / MW"), limits=range(ybreaks)) +
					#ylab(expression(paste(X[it], ' and ' , X(t), ': Capacity Price in Euro/MW/w'))) + # 
					ylab("Capacity Price in Euro/MW/w") + 
					xlab(expression(paste(x[it], ': Cumulated Capacity'))) +
					ggtitle(paste("Capacity Price NEG_HT ", format(bid_data$dates[curve_index], "%Y-%m-%d"), sep="")) +
					theme(axis.title.y = element_text(size = rel(1.2), angle = 90)) +
					theme(axis.title.x = element_text(size = rel(1.2), angle = 00)) +
					theme(plot.title = element_text(lineheight=3, face="bold", color="black", size=15))+ 
					theme(plot.title = element_text(hjust = 0.5)) +
					theme(legend.position="right"))
					
dev.off()

   

##########################################################################################
## Bin Plots							     											##
##########################################################################################
## Bin Plot depending on lp Resolution (Intervalls = 2*lpResolution, mwLevelSeq for coloring; maximum 9 Intervalls)
## bidData of the structure from before with dates as bidData$dates and lps as bidData$lps
## color has to be something of colorbrewer
## in case of weeks
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)



##  in case of hours:
## lpResolution <- 0.2
## types: type="bin", type="bidSize", type="mwPosition" 
mwLevelSeq 				<- c(seq(0,180,30),3000)
Xticks 					<- getXTicks(biddata[[1]]$quantiles$dates)
# hightlight christmas demand change:
dates_christmas_df 		<- as.data.frame(cbind(	event = c("Christmas 2013", "Christmas 2014"), 
                                             startDate = c("2013-12-21", "2014-12-20"), #two days before first acution
                                             endDate = c("2014-01-01", "2014-12-31")), # four days after second auction
                                      rownames = NULL, stringsAsFactors = FALSE)
dates_christmas_df$startDate <- as.Date(dates_christmas_df$startDate)
dates_christmas_df$endDate 	<- as.Date(dates_christmas_df$endDate)

lpResolutions 			<- c(	"POS_HT" = 5, 
								"POS_NT" = 5,
								"NEG_HT" = 20,
								"NEG_NT" = 20)
mwLevelSeq   			<- c(seq(0,180,30),3000)
max_price    			<- c(	"POS_HT" = 1500, 
                      "POS_NT" = 2500,
                      "NEG_HT" = 3000,
                      "NEG_NT" = 3200)

for(pmhtnt in PMHTNT){
		print(pmhtnt) # pmhtnt <- "NEG_HT"
        x         				<- biddata[[pmhtnt]] # bidData
        
		# str(biddata[[pmhtnt]], max = 1)
		# biddata[[pmhtnt]]$quantiles$gavg
		
        Xticks    				<- getXTicks(x$quantiles$dates)
        filename  				<- paste0(rla, "_", pmhtnt)
        cust      				<- customizeProduct(filename) # customize colours
        
        assign( 	x = 	paste0("binplot_", pmhtnt), 
                 value = plotBinPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
                                 filename = filename, pricetyp = pricetype, max_price = max_price[pmhtnt], ymax = max_price[pmhtnt],
                                 rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F, ggplottitle = NULL, ylab = pmhtnt) ) 
        # binplot_POS_HT
		# 
        # assign( 	x = 	paste0("binplot_", pm, "_",htnt), 
        # value = getPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
        # filename = filename, pricetype, max_price = max_price[paste0(pm, "_",htnt)],
        # rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F) +
        # geom_rect(	data = dates_christmas_df, # add grey shades
        # aes(NULL, NULL, xmin = startDate, xmax = endDate, colour = NULL), # unmap colour
        # ymin = -Inf, ymax = +Inf, fill = "gray80", alpha = 0.8))  
    
} # now four objects for each combinations of pm and htnt, e.g.:
binplot_POS_HT <- binplot_POS_HT + theme(legend.position = "none", axis.text.x=				element_blank()) 
binplot_POS_NT <- binplot_POS_NT + theme(legend.position = "top") + guides(colour = 		guide_legend(nrow = 1))
binplot_NEG_HT <- binplot_NEG_HT + theme(legend.position = "none", axis.text.x=				element_blank())
binplot_NEG_NT <- binplot_NEG_NT + theme(legend.position = "top")  + guides(colour = 		guide_legend(nrow = 1))
pdf(paste0("C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\", format(Sys.time(), format="%Y%m%d"), "bin_plot_all_vertical.pdf"), width = 17, height = 26)
grid.arrange(	binplot_POS_HT, binplot_POS_NT, 
            binplot_NEG_HT, binplot_NEG_NT, nrow=4, ncol = 1, heights = c(5, 8,5,8),
			  top=textGrob("Capacity Price in \u20ac/MW/w",gp=gpar(fontsize=20,font=1)))
dev.off()

# ggsave(filename = "C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\bin_plot_ALL.pdf",
       # plot = binplot_NEG_HT+ theme(legend.position = "bottom"), width = 30, height = 20, units = "cm")
##########################################################################################	   


##########################################################################################
## Bin Plot full for NEG HT 
library(gridExtra)
library(grid)
library(ggplot2)
library(lattice)



##  in case of hours:
## lpResolution <- 0.2
## types: type="bin", type="bidSize", type="mwPosition" 
mwLevelSeq 				<- c(seq(0,180,30),3000)
Xticks 					<- getXTicks(biddata[[1]]$quantiles$dates)
# hightlight christmas demand change:
dates_christmas_df 		<- as.data.frame(cbind(	event = c("Christmas 2013", "Christmas 2014"), 
                                             startDate = c("2013-12-21", "2014-12-20"), #two days before first acution
                                             endDate = c("2014-01-01", "2014-12-31")), # four days after second auction
                                      rownames = NULL, stringsAsFactors = FALSE)
dates_christmas_df$startDate <- as.Date(dates_christmas_df$startDate)
dates_christmas_df$endDate 	<- as.Date(dates_christmas_df$endDate)


mwLevelSeq   			<- c(seq(0,180,30),3000)
max_price    			<- c(	"POS_HT" = FALSE, 
                      "POS_NT" = 2500,
                      "NEG_HT" = 24000,
                      "NEG_NT" = 3200)
lpResolutions 			<- c(	"POS_HT" = 5, 
                      "POS_NT" = 5,
                      "NEG_HT" = 20,
                      "NEG_NT" = 20)
for(pmhtnt in PMHTNT){
		print(pmhtnt) # pmhtnt <- "NEG_HT"
        x         				<- biddata[[pmhtnt]] # bidData
        lpResolution 			<- lpResolutions[pmhtnt]
		# str(biddata[[pmhtnt]], max = 1)
		# biddata[[pmhtnt]]$quantiles$gavg
		
        Xticks    				<- getXTicks(x$quantiles$dates)
        filename  				<- paste0(rla, "_", pmhtnt)
        cust      				<- customizeProduct(filename) # customize colours
        
		print(maxPrice 			<- max(x$quantiles$max))
		
        assign( 	x = 	paste0("binplot_max_", pmhtnt), 
                 value = plotBinPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
                                 filename = filename, pricetyp = pricetype, max_price = max_price[pmhtnt], ymax = max_price[pmhtnt],
                                 rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F, ggplottitle = NULL) ) 
        # binplot_POS_HT
		# 
        # assign( 	x = 	paste0("binplot_", pm, "_",htnt), 
        # value = getPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
        # filename = filename, pricetype, max_price = max_price[paste0(pm, "_",htnt)],
        # rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F) +
        # geom_rect(	data = dates_christmas_df, # add grey shades
        # aes(NULL, NULL, xmin = startDate, xmax = endDate, colour = NULL), # unmap colour
        # ymin = -Inf, ymax = +Inf, fill = "gray80", alpha = 0.8))  
    
} # now four objects for each combinations of pm and htnt, e.g.:


ggsave(filename = "C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\bin_plot_NEG_HT_max.pdf",
       plot = binplot_max_NEG_HT + theme(legend.position = "bottom"), width = 30, height = 20, units = "cm")
##########################################################################################	   

	