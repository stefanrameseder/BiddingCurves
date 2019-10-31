# get the number of bids

# get the number of bids
prepareData <- function(quantilePath, bidPath, rla, pm, pricetype, apLoss=380, gridPoints = 1){
  # input:
  # quantilePath: path for quantile data
  # bidPath: path for bid data
  # rla: SRL or MRL; LP or AP are included into bidpath
  # pm: POS or NEG
  # pricetype: "LP" or "AP"; important for data choice as well
  
  
  #assign(x = paste0("bid_data_", i, "_",j), value = prepareData(quantilePath=paste0("data2/", rla, "_", i, "_",j,"_", pricetype,"_quantiles.csv"), bidPath=paste0("data2/", rla, "_", i, "_",j,"_", pricetype,".csv"), rla = rla,pm = pm, apLoss=380)) 
  #quantilePath=paste0("data2/SRL_POS_HT_AP_quantiles.csv")
  #bidPath=paste0("data2/SRL_POS_HT","_AP.csv")
  
  quantiles 			<- read.table(quantilePath, header=TRUE, sep=";", dec=",") # mit den quantilen
  quantiles 			<- na.omit(quantiles[,2:6])
  print(quantilePath)
  
  quantiles$spread 		<- quantiles$max - quantiles$min
  quantiles$uspread 	<- quantiles$max - quantiles$gavg
  quantiles$lspread 	<- quantiles$gavg - quantiles$min
  
  
  
  bids 					<- read.table(bidPath, header=TRUE, sep=";", dec=",", na.strings="") #bids
  #summary(bids) dim(bids) bids <- bids[, -421]
  mws					<- getMWDataOfBidCurves(bids)								
  
  mws_zero 				<- apply(mws, 2, function(x) x-min(x, na.rm = TRUE)) # 
  
  # max(apply(mws_zero, 2, max, na.rm = TRUE))
  
  price					<- getLPOfBidCurves(bids)
  
  bidSizes 				<- getBidSizesOfBidCurves(mws)
  nBids 				<- getNumberOfBids(price)
  
  dates 				<- getDatesOfBidCurves(bids)

  quantiles 			<- cbind(dates=dates, quantiles)
  
  # as.numeric(as.numeric(format(quantiles$dates, "%m"))==12 & 
               # as.numeric(format(quantiles$dates, "%d"))>=17)
  quantiles$christmas 	<- as.numeric(as.numeric(format(quantiles$dates, "%m"))==12 & as.numeric(format(quantiles$dates, "%d"))>=17)
  
  
  nHolidays 			<- read.csv("data/holidays.csv", header=FALSE, sep=";")[,1:2]
  nHolidays 			<- nHolidays[(1:length(dates)),]
  
  factor 				<- cbind(as.Date(quantiles$dates), nHolidays[,2], 60-nHolidays[,2]*12)
  
  #factor <- cbind(as.Date(bidData$quantiles$dates), nHolidays[,2], 60-nHolidays[,2]*12)
  price_hour 			<- matrix(NA, nrow=dim(price)[1], ncol=dim(price)[2])
  
  for(k in 1:length(factor[,3])){ 
    price_hour[,k] 			<- round(price[,k]/factor[k,3],1)
  }
  
  gavgh 				<- quantiles$gavg/factor[,3]
  maxh 					<- quantiles$max/factor[,3]
    
  #bidData$quantiles$gavg
  
  ############################ Demanded Capacity
	dates_mw 			<- as.Date(c(	"27.06.2011", 	"03.10.2011", 	"02.01.2012",	"02.04.2012",	"30.07.2012",
										"01.10.2012",	"07.01.2013",	"01.04.2013",	"01.07.2013",	"30.09.2013",	
										"23.12.2013",	"04.01.2014",	"31.03.2014",	"07.07.2014",	"29.09.2014",
										"22.12.2014", 	"31.12.2014", 	"30.03.2015", 	"13.04.2015", 	"29.06.2015",
										"28.09.2015", 	"21.12.2015",	"04.01.2016", 	"04.04.2016", 	"04.07.2016",
										"03.10.2016", 	"19.12.2016", 	"02.01.2017", 	"03.04.2017" ), 
										format="%d.%m.%Y")
 
  length(dates_mw)
  
  PRL_mw <- c(612,  612,	567,	592,	592,	
              592,	576,	576,	576,	576,	
              576,	628,	628,	628,	663, 
              670,  670,  	670,  	783,  	783,
			  783, 	795, 	793,	793,	793,
			  827, 	821, 	1400,	1404)
  names(PRL_mw)<-dates_mw # length(SRL_POS_mw)
  # in MRL ist der Rücksprung nach Weihnachten früher als in der SRL
  SRL_POS_mw <- c(2101, 2073,	2084,	2081,	2097,	
                  2109,	2133,	2136,	2091,	2073,	
                  2473,	2042,	1998,	1992,	2113, 
                  2500, 2054, 	2026, 	2026, 	2076,
				  2057, 2500, 	2054, 	1973, 	2031,
				  1976, 1976, 	1902,	1913)
				  
  names(SRL_POS_mw)<-dates_mw
  
  SRL_NEG_mw <- c(2074, 2044,	2114,	2127,	2136,	
                  2149,	2108,	2095,	2043,	2018,	
                  2418,	1998,	1919,	1906,	2057, 
                  2500, 2021, 	1973, 	1973, 	2013,
				  2002, 2500, 	1979, 	1904,	1962,
				  1902, 2250, 	1832,	1846)
  names(SRL_NEG_mw)<-dates_mw
  
  MRL_POS_mw <- c(1864, 1812, 	1737,	1552,	2075,	
                  2426,	2406,	2434,	2593,	2447,	
                  2947,	2472,	2464,	2476, 	2083,
                  2100, 2123, 	2726, 	2726, 	1513,
				  1777, 2100, 	2101,	2779,	2779,
				  1504, 1850,	1257, 	1506)
  names(MRL_POS_mw)<-dates_mw
  
  MRL_NEG_mw <- c(2509, 2491,	2158,	2371,	2371,	
                  2413,	2452,	2423,	2716,	2720,	
                  3220,	2838,	2801,	2208,	2184, 
                  3000, 2522, 	2039, 	2039, 	1782,
				  2211, 2500, 	2353,	2006,	1715,
				  1654,	1800, 	1926, 	1072)
  names(MRL_NEG_mw)<-dates_mw
  demCap <- list(PRL_mw=PRL_mw, SRL_POS_mw=SRL_POS_mw, 
                 SRL_NEG_mw=SRL_NEG_mw, MRL_POS_mw=MRL_POS_mw, MRL_NEG_mw=MRL_NEG_mw)
  
  
  ############################ Clean LP Bidcurves in mws_clean
  #cbind(rbind(0,mws[, c(1,2,3,4,5)]), mwsc[, c(1,2,3,4,5)])
  mwsc <- rbind(0, mws)
  price_clean <- rbind(price[1,], price)
  #dates < dates_mw[2]
  #dates[133]

  
 

  demCap_dates <- numeric(length(dates))
  for(i in 1:length(dates)){ 
    (corDate <- max(dates_mw[dates_mw<=dates[i]]))
    #corDate
    maxCapacity <- get(paste0(rla, "_", pm, "_mw"))[as.character(corDate)]
    demCap_dates[i]<-maxCapacity
    #maxCapacity
    #### change starting point of mws_clean to zero:
    mwsc[which.max(mwsc[,i]),i] <- maxCapacity
  }
  bidSizesc <- getBidSizesOfBidCurves(mwsc)
  
  mwsc_zero	<- apply(mwsc, 2, function(x) x-min(x, na.rm = TRUE))
  #cbind(rbind(0,mws[, c(1,2,3,4,5)]), mwsc[, c(1,2,3,4,5)])
  
  
  ### Get for each MW in mwGrid the corresponding price, i.e.
  # (1, lp_1), (2, lp_1), ... (11, lp_2),
  # matplot(x = mwGrid, binMat, type = "l")
  maxMW		<- max(demCap[[paste0(rla, "_", pm, "_mw")]])
  mwGrid	<- seq(1, maxMW, by = gridPoints)
  binMat 	<- matrix(NA, nrow = maxMW, ncol = dim(mwsc_zero)[2])
  
  for(date in 1:dim(mwsc_zero)[2]){ # date <- 1
	for(bin in mwGrid){# bin = 2102
		if( sum(bin <= mwsc[ ,date], na.rm = TRUE ) > 0 ){
			#print("HERE1")
			binMat[bin,date] <- price_clean[ min(which( bin <= mwsc[ ,date] )), date]
		} else {
			#print("HERE2")
			binMat[bin,date] <- NA
		}
		#print(paste0("Bin: ", bin, " and Date: ", date, " Price: ", binMat[bin,date]))
		#cat("\n")
	}
  }
  
  
  
  return(list(quantiles=quantiles, bids=bids, dates=dates, mws=mws, price=price, mws_zero = mws_zero,
              bidSizes=bidSizes, bidSizesc=bidSizesc, nBids=nBids, apLoss =apLoss, price_hour=price_hour, gavgh=gavgh, 
              maxh = maxh, dates_mw=dates_mw, demCap = demCap, mwsc = mwsc, price_clean = price_clean, maxMW = demCap_dates, 
			  holidays=nHolidays, mwsc_zero=mwsc_zero, binGrd = mwGrid, binMat = binMat))
}
