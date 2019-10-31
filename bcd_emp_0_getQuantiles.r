print(getwd())

for(pricetype in PriceTypes){
	for(pm in PM){ # pm <- PM[1]
	  for(htnt in HTNT){ # htnt <- HTNT[1]
		print(pmhtnt <- paste0(pm, "_", htnt))

			bidPath 			<- paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,".csv")
			
			bids 				<- read.table(bidPath, header=TRUE, sep=";", dec=",", na.strings="") #bids
			#summary(bids) dim(bids) bids <- bids[, -421]
			mws					<- getMWDataOfBidCurves(bids)								
			# str( , max = 1)
			mws_zero 			<- apply(mws, 2, function(x) x-min(x, na.rm = TRUE)) # 

			# max(apply(mws_zero, 2, max, na.rm = TRUE))
			  
			price				<- getLPOfBidCurves(bids)

			bidSizes 			<- getBidSizesOfBidCurves(mws)
			nBids 				<- getNumberOfBids(price)

			dates 				<- getDatesOfBidCurves(bids)
			
			
			(max 	<- apply(price, 2, max, na.rm = TRUE))
			(min 	<- apply(price, 2, min, na.rm = TRUE))
			(median <- apply(price, 2, median, na.rm = TRUE))
			T		<- length(max)
			gavg 	<- numeric(T)
			for(t in 1:T){ # t <- 2
				#print(t)
				gavg[t] <-  na.omit(price[ , t]) %*%  na.omit(bidSizes[ ,t]) / sum(na.omit(bidSizes[ ,t]))
			}
		
			ezgw 	<- numeric(T)
			for(t in 1:T){ # t <- 2
				ezgw[t] <-  price[ which.min(na.omit(mws[ ,t] < 1200)), t]
			}
			
			efgw 	<- numeric(T)
			for(t in 1:T){ # t <- 2
				efgw[t] <-  price[ which.min(na.omit(mws[ ,t] < 1500)), t]
			}
			
			df 	<- data.frame(date= format(dates, "%d.%m.%Y"), max = max, efg = efgw, ezgw = ezgw, gavg = gavg, min = min, median = median)
			
			write.table(df, file = paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,"_quantiles.csv"), sep = ";",
				eol = "\n", na = "NA", dec = ",", row.names = FALSE)
			
		}
	}
}