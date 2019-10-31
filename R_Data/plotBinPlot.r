### Bin Plot
##############################################################################################
##############################################################################################
plotBinPlot <- function(lpResolution, mwLevelSeq, bidData,  color, type="bin", filename, pricetyp, 
                    max_price=FALSE, rev=FALSE, pch=15, pointSize=1, h=FALSE, ymax = FALSE)
{
  if(length(mwLevelSeq) >= 10)
  {
    print("Fehler bei der Anzahl der MW Intervalle")
    return(NA)
  }
  
  if(type=="bin")
  { 
    if(isTRUE(!rev))
    {
      #color=cust$color[1]
      #rev=as.logical(cust$color[2])
      pal <-brewer.pal(9, color)[(9-(length(mwLevelSeq)-2)):9]
    }
    else
    {
      pal <- rev(brewer.pal(9, color))[(9-(length(mwLevelSeq)-2)):9]
    }
    
    if(isTRUE(!h)) # falls h = FALSE -> !h = TRUE und man berechnet Wochenlps
    {
      
      if(!max_price){  # max = FALSE; then max is calculated accordingly to bidData
        lpLevelSeq <- seq(lpResolution, max(bidData$quantiles$max)+4*lpResolution , lpResolution*2)
      }
      else{ # max is given by the user
        lpLevelSeq <- seq(lpResolution, max_price+4*lpResolution , lpResolution*2)
      }
      
      gavgDF <- data.frame(x=bidData$dates, y=bidData$quantiles$gavg)
      
      lpsBinsPlotMat <- matrix(NA, ncol=dim(t(bidData$price))[2], nrow=dim(t(bidData$price))[1])
      lpsBinsPlotCol <- matrix(NA, ncol=dim(t(bidData$price))[2], nrow=dim(t(bidData$price))[1])
      
      for(i in 1:dim(t(bidData$price))[1]){
        #(i <- which.max(bidData$quantiles$max))
        
        lpsBins <- cut( t(bidData$price)[i,] , breaks=(lpLevelSeq-lpResolution) )
        
        levels(lpsBins) <- lpLevelSeq
        
        uniqueBins <- na.omit(unique(as.numeric(levels(lpsBins))[lpsBins]))
        
        
        
        lpsBinsPlotMat[i, 1:length(uniqueBins)] <- uniqueBins
        
        counter <- 1
        
        for(k in uniqueBins) 
        {
          #(k <- uniqueBins[1])
          mwSum <- sum(na.omit(t(bidData$bidSizes)[i,lpsBins==k]))
          #mwSum
          lpsBinsPlotCol[i, counter] <- mwSum
          counter <- counter+1
        }
      }
      
      if(filename=="SRL_POS_HT")
      {	
		if(ymax == FALSE){
			filenameBreaks=seq(0, max(bidData$quantiles$max), 200)
		} else{
			filenameBreaks=seq(0, ymax, length.out = 10)
		}
        
      }
      else
      {
        if(ymax == FALSE){
			filenameBreaks=seq(0, max(bidData$quantiles$max), 200)
		} else{
			filenameBreaks=seq(0, ymax, length.out = 10)
		}
      }
      
      #title <- paste0("Auktionsergebnisse ", paste(unlist(strsplit(filename, "[_]")), collapse=" "), " ", pricetyp)
      #subtitle <- paste0("Leistungsgewichtete Preise der zugeschlagenen Gebote (",
       #                  format(min(bidData$quantiles$dates), format="KW %U / %Y"), " - ",
        #               format(max(bidData$quantiles$dates+6), format="KW %U / %Y"), ")")
      
      #title <- paste0("Capacity Bid Curves ", paste(unlist(strsplit(filename, "[_]")), collapse=" "))
      #subtitle <- paste0("Capacity Prices of accepted Bids (",
                         #format(min(bidData$quantiles$dates), format="Week %U / %Y"), " - ",
                         #format(max(bidData$quantiles$dates+6), format="Week %U / %Y"), ")")
      
      #title <- ""
      #subtitle <- ""
      
      lpsBinsMELT <- melt( data.frame(dates=bidData$dates, lpsBinsPlotMat), id.vars = "dates")
      mwSizesMELT <- melt( data.frame(dates=bidData$dates, lpsBinsPlotCol), id.vars = "dates")
      lpsBinsMELT$BidSize <- cut(mwSizesMELT$value
                                 , breaks=mwLevelSeq)
      levels(lpsBinsMELT$BidSize) <- paste0("< ", mwLevelSeq[2:length(mwLevelSeq)])
      
      if(pricetyp=="LP"){
        ylab <- "Capacity Price in \u20ac per MW per Week"
      }
      if(pricetyp=="AP"){
        ylab <- "Arbeitspreis in \u20ac pro MWh"
      }
      
      plot <- ggplot(data=lpsBinsMELT, aes(x = dates, y = value, colour=BidSize)) +
        geom_point(size=pointSize, shape=pch) +
        ylab(ylab) +
        scale_colour_manual(name = "MW Intervall",values = pal) + 
        scale_y_continuous(breaks=filenameBreaks) + # neg nt case
        theme_bw() +
        #ggtitle(bquote(atop(.(title), atop(.(subtitle),""))))+
        
        
        #ggtitle(expression(atop(paste0("Auktionsergebnisse ", paste(unlist(strsplit(filename, "[_]")), collapse=" ")), "LP \nLeistungsgewichtete Preise der zugeschlagenen Gebote (", 
                     #format(min(bidData$quantiles$dates), format="KW %U / %Y"), " - ",
                     #format(max(bidData$quantiles$dates+6), format="KW %U / %Y"), ")"), atop("TEST"), " "))  + # since on monday 0:00 it is the old kw
        theme(plot.title = element_text(face="bold"), axis.text.x = element_text(angle=45, hjust = 0.9), axis.title.x = element_blank()) +
        scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks("4 months")) +
        guides(colour = guide_legend(override.aes = list(alpha = 1, size=4))) +
        geom_path(data=gavgDF, aes(x=x,y=y), colour="black", size=0.3, alpha=0.5) 
      return(plot)
    } 
    else # Leistungspreise auf Stundenbasis
    {
      # lpLevelSeq 
      #max(apply(bidData$lpsh, 2, max, na.rm=TRUE))
      #lpLevelSeq <- seq(0, 40, 5)
      #pointSize = 1
      #pch=15
      #lpResolution <- 0.2
      #str(bidData, max=1)
      #bidData$lpsh[,2]
      #bidData$gavgh
      bidData <- bid_data
      #lpResolution <- 0.2
      lpLevelSeq <- seq(lpResolution, max(bidData$quantiles$max)+4*lpResolution , lpResolution*2)
      #lpLevelSeq
      gavgDF <- data.frame(x=bidData$dates, y=bidData$gavgh)
      
      lpsBinsPlotMat <- matrix(NA, ncol=dim(t(bidData$lpsh))[2], nrow=dim(t(bidData$lpsh))[1])
      lpsBinsPlotCol <- matrix(NA, ncol=dim(t(bidData$lpsh))[2], nrow=dim(t(bidData$lpsh))[1])
      
      
      #lpResolution <- 0.2
      for(i in 1:dim(t(bidData$lpsh))[1])
      {
        
        lpsBins <- cut( t(bidData$lpsh)[i,] , breaks=(lpLevelSeq-lpResolution))
        bidData$lpsh[,186]
        
        
        levels(lpsBins) <- lpLevelSeq
        uniqueBins <- na.omit(unique(as.numeric(levels(lpsBins))[lpsBins]))
        
        lpsBinsPlotMat[i, 1:length(uniqueBins)] <- uniqueBins
        
        counter <- 1
        
        for(k in uniqueBins) 
        {
          #(k <- uniqueBins[1])
        
          mwSum <- sum(na.omit(t(bidData$bidSizes)[i,lpsBins==k]))
          #mwSum
          lpsBinsPlotCol[i, counter] <- mwSum
          counter <- counter+1
        }
      }
      #lpsBinsPlotMat[6,]
      #lpsBinsPlotCol[6,]
      #bidData$lpsh[,6]
      
      #data.frame(dates=bidData$dates, lpsBinsPlotMat)
      #lpsBinsMELT[lpsBinsMELT$dates=="2011-08-01",]
      #names(lpsBinsMELT$dates=="2011-08-01")
      #lpsBinsMELT$dates
      lpsBinsMELT <- melt( data.frame(dates=bidData$dates, lpsBinsPlotMat), id.vars = "dates")
      mwSizesMELT <- melt( data.frame(dates=bidData$dates, lpsBinsPlotCol), id.vars = "dates")
      lpsBinsMELT$BidSize <- cut(mwSizesMELT$value
                                 , breaks=mwLevelSeq)
      levels(lpsBinsMELT$BidSize) <- paste0("< ", mwLevelSeq[2:length(mwLevelSeq)])
      
      title <- paste0("Capacity Bid Curves ", paste(unlist(strsplit(filename, "[_]")), collapse=" "))
      subtitle <- paste0("Capacity Prices of accepted Bids (",
                         format(min(bidData$quantiles$dates), format="Week %U / %Y"), " - ",
                         format(max(bidData$quantiles$dates+6), format="Week %U / %Y"), ")")
      
     
      if(pricetyp=="LP"){
        ylab <- "Capacity Price in \u20ac/MW/h"
      }
      if(pricetyp=="AP"){
        ylab <- "Arbeitspreis in \u20ac pro MWh"
      }
      
     
      
      plot <- ggplot(data=lpsBinsMELT, aes(x = dates, y = value, colour=BidSize)) +
        geom_point(size=pointSize, shape=pch) +
        ylab(ylab) +
        scale_colour_manual(name = "BidSize",values = pal) + 
        scale_y_continuous(breaks=seq(0, max(bidData$maxh), 4), limits=c(0,50)) +
        theme_bw() +
        ggtitle(bquote(atop(.(title), atop(.(subtitle),""))))+# since on monday 0:00 it is the old kw
       
        theme(plot.title = element_text(face="bold"), axis.text.x = element_text(angle=45, hjust = 0.9), axis.title.x = element_blank()) +
        scale_x_date(labels = date_format("%m-%Y"), breaks = date_breaks("4 months")) +
        guides(colour = guide_legend(override.aes = list(alpha = 1, size=4))) +
        geom_path(data=gavgDF, aes(x=x,y=y), colour="black", size=0.3, alpha=0.5)
      plot
      
      
      return(plot)
    } 
  }
  else if(type=="bidSize")
  {
    lpsDF <- data.frame(dates=bidData$dates, t(bidData$price))
    
    lpsMELT <- melt(lpsDF, id.vars = "dates")
    
    mwSizes <- data.frame(dates=bidData$dates, t(bidData$bidSizes))
    
    mwSizesMELT <- melt(mwSizes, id.vars = "dates")
    
    lpsMELT$BidSize <- cut(mwSizesMELT$value
                           , breaks=c(0,10,20,30,40,50, 60, 500))
    
    plot <- ggplot(data=lpsMELT, aes(x = dates, y = value, colour=BidSize)) +
      geom_point(pointSize=1) +
      xlab("Auktionswoche") + ylab("Leistungspreis") +
      scale_colour_manual(name = "BidSize",values = brewer.pal(8, color))
    return(plot)
  }
  else if(type=="mwPosition")
  {
    lpsDF <- data.frame(dates=bidData$dates, t(bidData$price))
    lpsMELT <- melt(lpsDF, id.vars = "dates")
    
    mwPos <- data.frame(dates=bidData$dates, t(bidData$mws))
    
    mwPosMELT <- melt(mwPos, id.vars = "dates")
    lpsMELT$group <- cut(mwPosMELT$value
                         , breaks=seq(0,2500, 300))
    
    
    levels(lpsMELT$group) <- paste0("< ", seq(0,2500, 300), "MW")
    p <- ggplot(data=lpsMELT, aes(x = dates, y = value, colour=group)) +
      geom_point(pointSize=1) +
      xlab("Auktionswoche") + ylab("Leistungspreis") +
      scale_colour_manual(name = "group",values = brewer.pal(length(seq(0,2500, 300)), color))
    p
  }
}

