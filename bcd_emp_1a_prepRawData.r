##########################################################################################  
##########################################################################################
## Preprocessing of bcd data via monotSpline

rm(list=ls())

user     	<- "stefan" 		# User Choice for Working Directory

if(user == "stefan"){
    if(Sys.info()[1]=="Darwin") {
        setwd("/Users/stefanrameseder/Google Drive/BCD/R_Code")
    } else {
        setwd("C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves")
    }
}


# Source Simulation DGP Data for DGP_name 
source("bcd_aaa_setDataAndLibraries.r")



##########################################################################################
# Import/Adjust Data and define date properties ##########################################
########################################################################################## 

biddata <- list()
for(pricetype in PriceTypes){
	for(pm in PM){ # pm <- PM[1]
		for(htnt in HTNT){ # htnt <- HTNT[1]

		print(pmhtnt <- paste0(pm, "_", htnt))
		biddata[[pmhtnt]] <- prepareData(quantilePath = paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,"_quantiles.csv"), 
											bidPath = paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,".csv"), 
											rla = rla, pm = pm, pricetype = pricetype, 
											apLoss=380)
		}
	}
	save(x = biddata, file = paste0("R_Data/",rla, "_", pricetype, "_biddata.RData"))
} # now four objects for each combinations of pm and htnt, e.g.: data_l


### Structure of Rawdata for each pmhtnt:
# $ quantiles  :'data.frame':    304 obs. of  10 variables:
# $ bids       :'data.frame':    127 obs. of  525 variables:
# .. [list output truncated]
# $ dates      : Date[1:262], format: "2011-06-27" "2011-07-04" "2011-07-11" "2011-07-18" ...
# $ mws        : int [1:127, 1:304] 10 20 25 50 75 90 100 110 125 135 ...
# $ price      : num [1:127, 1:304] 450 460 476 500 525 ...
# $ mws_zero   : int [1:127, 1:304] 0 10 15 40 65 80 90 100 115 125 ...
# $ bidSizes   : num [1:127, 1:304] 10 10 5 25 25 15 10 10 15 10 ...
# $ bidSizesc  : num [1:128, 1:304] 0 10 10 5 25 25 15 10 10 15 ...
# $ nBids      : int [1:304] 60 57 68 71 74 41 73 46 68 72 ...
# $ apLoss     : num 380
# $ price_hour : num [1:127, 1:304] 7.5 7.7 7.9 8.3 8.8 9.1 9.2 9.5 9.6 10 ...
# $ gavgh      : num [1:304] 14 14.3 14.1 13.8 13.2 ...
# $ maxh       : num [1:262] 17.3 15.6 15.1 14.4 14 ...
# $ dates_mw   : Date[1:25], format: "2011-06-27" "2011-10-03" "2012-01-02" "2012-04-02" ...
# $ demCap     :List of 5
# $ mwsc       : num [1:128, 1:262] 0 10 20 25 50 75 90 100 110 125 ...
# $ price_clean: num [1:128, 1:262] 450 450 460 476 500 ...
# $ maxMW      : num [1:262] 2101 2101 2101 2101 2101 ...
# $ holidays   :'data.frame':    262 obs. of  2 variables:
# $ mwsc_zero  : num [1:128, 1:262] 0 10 20 25 50 75 90 100 110 125 ...
# $ binGrd     : num [1:2500] 1 2 3 4 5 6 7 8 9 10 ...
# $ binMat     : num [1:2500, 1:262] 450 450 450 450 450 450 450 450 450 450 ...

##########################################################################################
# Save rawdata in a list of length T with (x_i, y_i) pairs ###############################
########################################################################################## 
for(pricetype in PriceTypes){ # pricetype <- PriceTypes[1]
	bidcurves <- list(dates = biddata[[pmhtnt]]$dates, POS_HT=list(), POS_NT = list(), NEG_HT = list(), NEG_NT = list())
	load(paste0("R_Data/",rla, "_", pricetype, "_biddata.RData"))
	for(pm in PM){
	  for(htnt in HTNT){
		pmhtnt 		<- paste0(pm, "_", htnt)
		
		
		data_mat	<- biddata[[pmhtnt]]
	
		for( t in 1:length(biddata[[pmhtnt]]$dates)){ # t <- 1
			bidcurves[[pmhtnt]][[t]] <- matrix(cbind(na.omit(data_mat$mwsc[ ,t]), na.omit(c(data_mat$price[1,t], data_mat$price[ ,t]))), ncol = 2)
		}
	  }
	}
	save(bidcurves, file = paste0("R_Data/",rla ,"_", pricetype ,"_bidcurves.RData"))
}
# str(data_mat$dates, max = 1);data_mat$dates

