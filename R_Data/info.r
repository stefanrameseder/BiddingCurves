# Variablen:
# rla            			<- "SRL"#Sekundärregelleistung oder Minutenreserveleistung
# pricetype       			<- "LP" # or "AP"  Leistungspreis oder Arbeitspreis
# pm						<- "POS" # or "NEG" Positive Richtung oder Negative Richtugn des Energieangebots
# htnt						<- "HT" # or "NT" für Hochtarif- oder Niedrigtarifzeitscheibe

# Es gibt für das DFM generell vier Zeitreihen: 

# SRL_LP_POS_HT
# SRL_LP_POS_NT
# SRL_LP_NEG_HT
# SRL_LP_NEG_NT

# Darüber hinaus können noch andere (später) benutzt werden!

# Es gibt für diese vier Zeitreihen drei (!) .RData-Dateien:
# - Rohdatensammlung: biddata
# - Rohdatenkurven: bidcurves
# - pen. montonone Splineschätzungen der Kurven: bidcurves_smoothed
library(ggplot2)
library(orthogonalsplinebasis)
setwd("C:\\Users\\LocalAdmin\\Google Drive\\BCD\\R_Code\\R_Data")
source("plotStepCurve.r")
source("plotBinPlot.r")
source("getXTicks.r")
source("customizeProduct.r")


rla            			<- "SRL"
PM   				  	<- c("POS", "NEG")
HTNT 				  	<- c("HT", "NT")
pricetype       		<- "LP" # "LP" or "AP"

##############################################################
##############################################################
# 1) biddata in SRL_LP_biddata.RData
# Eine Sammlung aller wichtigen Variablen:
### Structure of Rawdata for each pmhtnt:
# $ quantiles  :'data.frame':    262 obs. of  10 variables:
# $ bids       :'data.frame':    127 obs. of  525 variables:
# .. [list output truncated]
# $ dates      : Date[1:262], format: "2011-06-27" "2011-07-04" "2011-07-11" "2011-07-18" ...
# $ mws        : int [1:127, 1:262] 10 20 25 50 75 90 100 110 125 135 ...
# $ price      : num [1:127, 1:262] 450 460 476 500 525 ...
# $ mws_zero   : int [1:127, 1:262] 0 10 15 40 65 80 90 100 115 125 ...
# $ bidSizes   : num [1:127, 1:262] 10 10 5 25 25 15 10 10 15 10 ...
# $ bidSizesc  : num [1:128, 1:262] 0 10 10 5 25 25 15 10 10 15 ...
# $ nBids      : int [1:262] 60 57 68 71 74 41 73 46 68 72 ...
# $ apLoss     : num 380
# $ price_hour : num [1:127, 1:262] 7.5 7.7 7.9 8.3 8.8 9.1 9.2 9.5 9.6 10 ...
# $ gavgh      : num [1:262] 14 14.3 14.1 13.8 13.2 ...
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
load("SRL_LP_biddata.RData")
str(biddata[["POS_HT"]], max = 1)
### All curves for one pm_htnt
matplot(x = biddata[["POS_HT"]]$binGrd, y = biddata[["POS_HT"]]$binMat, type = "l") 

### All demanded capacities
bid_data <- biddata[["POS_HT"]]
plot(stepfun(bid_data$dates_mw,c(bid_data$demCap$SRL_POS_mw[1],bid_data$demCap$SRL_POS_mw)), ylim=c(0, 3500),
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

##############################################################
##############################################################
# 2) bidcurves in SRL_LP_bidcurves.RData
# Eine Liste der Länge T, deren Listeneinträge alle 2-Tupel (x_i, y_i), i = 1, ..., n(t) beinhalten
load("SRL_LP_bidcurves.RData")
str(bidcurves[["POS_HT"]], max = 1)
bidcurve_idx 		<- 10
plot(x = bidcurves[["POS_HT"]][[bidcurve_idx]][ ,1], y = bidcurves[["POS_HT"]][[bidcurve_idx]][ ,2], type = "l") 

##############################################################
##############################################################
# 3) bidcurves_smoothed in SRL_LP_bidcurves_smoothed.RData
# Eine Liste der Länge T, deren Listeneinträge die P-Spline (= unconstrained) und mon. P-Spline Schätzungen (constrained), 
# deren Evaluierungen (... .eval) sowie die Kurven, die evaluiert werden können ... .curve enthalten.
load("SRL_LP_bidcurves_smoothed.RData")
str(bidcurves_smoothed[["POS_HT"]][[1]], max = 1)
## Plot with function defined below

n_ev_points					<- 100
x_grid		 				<- seq(from = 0, to = 200, length.out = n_ev_points)
y_smoothed  				<- bidcurves_smoothed[["POS_HT"]][[bidcurve_idx]]$constrained.curve(x_grid, deriv = 0)
smooth_data 				<- data.frame(y = y_smoothed, x = x_grid	)
plotStepCurve(bid_data = biddata[["POS_HT"]], curve_index = bidcurve_idx, smooth=TRUE, smooth_data=smooth_data , point_col=rgb(174/255, 167/255, 0))



##############################################################
##############################################################
# Cenerate a timeseries object for a grid_distance parameter

# Maximum Domain = common Domain where all functions are observed
(comDoms 				<- sapply(bid_data$demCap, function(x) min(x))[c("SRL_POS_mw", "SRL_NEG_mw")])
names(comDoms) 		<- c("SRL_POS", "SRL_NEG")

gridDist 			<- 20 # 20 MW 

# Generate grids depending on a gridDist
(evalGrids 			<- sapply(comDoms, function(x) return(c(1, seq(from = gridDist, to = x, by = gridDist), x)))) # Grids have different lengths

# Generate a new list combining the true data bids (not the smoothed ones) to evaluate it at the gridDist
T 					<- length(biddata[[1]]$dates)
grdData 			<- lapply(biddata, function(x) list(binGrd = x$binGrd, binMat = x$binMat))   
for(pm in PM){
	for(htnt in HTNT){
		pmhtnt 		<- paste0(pm, "_", htnt)
		rlapm		<- paste0(rla, "_", pm)
		grdData[[pmhtnt]][["comDom"]] 		<- comDoms[[rlapm]]
		grdData[[pmhtnt]][["evalGrid"]] 	<- evalGrids[[rlapm]]
		grdData[[pmhtnt]][["grdMat"]] 		<- grdData[[pmhtnt]]$binMat[ evalGrids[[rlapm]] , ] # a length(evalGrid) x T Matrix
	}
}

grdData[[pmhtnt]][["grdMat"]] # this is a length(evalGrid) x T Matrix now, i.e. length(evalGrid) many time series of length T

matplot(x = 1:T, y = t(grdData[[pmhtnt]][["grdMat"]]), type = "l")


# Compare to BinPlot
filename			<- paste0(rla, "_", pm, "_", htnt)
lpResolution 		<- 5
mwLevelSeq 			<- c(seq(0,180,30),3000)
Xticks 				<- getXTicks(biddata[[pmhtnt]]$quantiles$dates)
# customize colors:
cust 				<- customizeProduct(filename)

(binPlot <- plotBinPlot(lpResolution, mwLevelSeq, biddata[[pmhtnt]], color=cust$color[1], type="bin", 
					filename = filename, pricetype, max_price = 3200, # Restriction of y maximum
					rev=as.logical(cust$color[2]), pch=15,pointSize=1, h=F) )


