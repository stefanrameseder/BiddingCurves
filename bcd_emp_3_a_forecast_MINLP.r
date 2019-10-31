


# Version for Saving the .RData
print(version 			<- paste0(ver,"_H", H, "_Bootstrap", Bootstrap, "_K", K, "_Mod_", model))

# make the cluster
cl <- makeCluster(8)
# load required libraries
clusterEvalQ(cl, {
    library(forecast)
    library(parallel)
    library(GenSA)
    library(timeSeries)
})
## export required objects
clusterExport(cl , varlist = c("H", "Bootstrap", "K", "forecastInd", "model", 
                               "dates", "bdQuan", "bdMax","bdMW","bdDem",
                               loadedFunctions, "biddata", # "bidcurves","bidcurves_smoothed", 
                               "maxit", "temp") )

set.seed(1)
#### Methodology: forecastCP in t means data up to t for a forecast for t+1
pmhtntFC <- pblapply(PMHTNT, function(pmhtnt){ # pmhtnt <- "POS_HT"
    
		
		# Define (lagged) matrices for all horizons:
		y			<- bdQuan[[pmhtnt]][ , c("min", "gavg", "max")] # y[1:2,]
		y1 			<- rbind(0, y[-T, ]) # y1[1:2,]
		y2 			<- rbind(0, y1[-T, ]) # y2[1:3,]
        y3 			<- rbind(0, y2[-T, ]) # y2[1:3,]
		y4 			<- rbind(0, y3[-T, ]) # y2[1:3,]
		y5 			<- rbind(0, y4[-T, ]) # y2[1:3,]
		y6 			<- rbind(0, y5[-T, ]) # y2[1:3,]
		
		# Choose the Matrix for the bidding function
		#yMAT 		<- as.matrix(cbind(y[ ,c("gavg", "max")], y1[ ,c("gavg", "max")], y2[ ,c("max")], y3[ ,c("max")], y4[ ,c("max")], y5[ ,c("max")])) 
		yMAT 		<- as.matrix(cbind(y[ ,c("gavg", "max")], y1[ ,"max"], y2[ ,"max"], y3[ ,"max"])) 
		
		yMAX		<- y[ , "max"]
		nPar		<- dim(yMAT)[2]
		low 		<- rep(-10, times = nPar)
		up 			<- rep(10, times = nPar)
		
		## export again
		clusterExport(cl , varlist = c("yMAT", "yMAX", "nPar", "low", "up", "maxit",
		                               "temp") )
		
		#sys.time <- system.time(fcs <- parLapply(cl, forecastInd, function(t){
		sys.time <- system.time(fcs <- lapply(forecastInd, get(model), 
									K = K, nPar = nPar, lossFunction = lossFunction, 
									rho = rho, yMAT = yMAT, yMAX = yMAX, 
									low = low, up = up, maxit=maxit, temp = temp))
    names(fcs) <- dates[forecastInd+1]
    print(file <- paste0("R_Data/bcd_",pmhtnt, "_", version,".RData"))
    #save(fcs, file = file)
	print(sys.time)
    return(fcs)
})


print(file <- paste0("R_Data/bcd_fc_minlp_", version,".RData"))
names(pmhtntFC) <- PMHTNT
save(pmhtntFC, file = file)
stopCluster(cl)					

load(file)

#### Evaluate methodology
sumUp <- summarizeFC(pmhtntFC, B, bdQuan, PMHTNT)


#### plot methodology
str(pmhtntFC[[pmhtnt]], max = 1)

fcPMHTNT    <- lapply(PMHTNT, function(pmhtnt) sapply(pmhtntFC[[pmhtnt]], function(fcs) fcs["fc"]))
names(fcPMHTNT) <- PMHTNT
maxPMHTNT   <- lapply(PMHTNT, function(pmhtnt) bdMax[[pmhtnt]][forecastInd+1])
names(maxPMHTNT) <- PMHTNT
gavgPMHTNT   <- lapply(PMHTNT, function(pmhtnt) bdQuan[[pmhtnt]][forecastInd+1, "gavg"])
names(gavgPMHTNT) <- PMHTNT
NotAccPMHTNT   <- lapply(PMHTNT, function(pmhtnt) sapply(pmhtntFC[[pmhtnt]], function(fcs) fcs["acc"]==0))
names(NotAccPMHTNT) <- PMHTNT

for(pmhtnt in PMHTNT){
    plot(forecastInd, fcPMHTNT[[pmhtnt]], col = "blue", type = "l")
    lines(forecastInd, gavgPMHTNT[[pmhtnt]], lty = 2)
    lines(forecastInd, maxPMHTNT[[pmhtnt]], lty = 1, col = "darkred", lwd = 2)
    points(x= forecastInd[NotAccPMHTNT[[pmhtnt]]], y= rep(0, times = length(forecastInd[NotAccPMHTNT[[pmhtnt]]])) , col = "red", pch = 16, cex =1)
    abline(v= forecastInd[NotAccPMHTNT[[pmhtnt]]], col = "darkred", lwd =0.5)
    abline(h=0, col ="black", lwd = 2)
    accAperfRate <- round(sumUp[pmhtnt, ],2)*100
    legend(	"topleft", col = c("blue", "darkred", "red", NA), inset = 0.01, 
            legend = c(paste0("Forecast (Performance: ", accAperfRate[2], "%)"), "Highest accepted price", paste0("# Non-accepted (", 100-accAperfRate[1], "%)")),
            lwd=c(2,1.5,2, NA), lty = "solid", bg = "white")
    readKey()
}


df	<- cbind(fc = fcPMHTNT[[pmhtnt]], max = maxPMHTNT[[pmhtnt]],  gavg = gavgPMHTNT[[pmhtnt]])

mult_ts <- timeSeries( data = df, 
                       charvec = dates[forecastInd],
                       format = "%Y-%m-%d")
#pdf(paste0("../latexGraphics/",pm,"_",htnt,".pdf"), width = 10, height = 8)
plot(mult_ts, 	plot.type="s", ylab = paste0(pmhtnt, " (EUR/MW)"), 
     #at = "pretty",  
     format = "%d\n%m\n%Y", at = seq(range(dates[forecastInd])[1], range(dates[forecastInd])[2], length.out=10), xlab = "Date", cex.axis = 1.25,
     col=c("blue", "black", "black"), lwd = c(2,1.5,2), lty = c(1,1,2))
names(NotAccPMHTNT[[pmhtnt]]) <- dates[forecastInd]
points(NotAccPMHTNT[[pmhtnt]]*10, col = "darkred", pch = 16, cex = 5)
points(fcPMHTNT[[pmhtnt]], col = "darkred", pch = 1, cex = 5)
abline(v=, col = "red")
points(dates[forecastInd][1], 800, col = "red")
accAperfRate <- round(sumUp[pmhtnt, ],2)
legend(	"topleft", col = c("blue", "black", "darkred", NA), inset = 0.01, 
        legend = c("Forecast", paste0("MaxAcc (", accAperfRate[2], "%)"), paste0("Acc = -2 (", accAperfRate[1], "%)")),
        lwd=c(2,1.5,2, NA), lty = "solid", bg = "white")
abline(h=0, col ="darkgrey")
 