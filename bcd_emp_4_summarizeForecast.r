##############################
##############################
##############################
# FDA 
# load(file = paste0("R_Data/completeFDA_ver_result_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))
fda_ver_result


completeFDA_FC_List <- do.call(c, unlist(fda_ver_result, recursive=FALSE))


allSummaries <- lapply(completeFDA_FC_List, function(x) x[["summary"]])
str(allSummaries, max = 1)


perPMHTNT_Summary_FDA <- lapply(PMHTNT, function(pmhtnt) sapply(allSummaries, function(x) x[pmhtnt, ]))
names(perPMHTNT_Summary_FDA) <- PMHTNT

print(FDA_results <- lapply(perPMHTNT_Summary_FDA, function(summary) { # summary <- perPMHTNT_Summary_FDA[[1]]
	accRateMaxInd <- which.max(summary[1, ])
	accRateName 	<- names(accRateMaxInd)
	perfRateMaxInd <- which.max(summary[2, ])
	perfRateName 	<- names(perfRateMaxInd)
	return(list(accRateName = accRateName, accRateMaxList = summary[, accRateMaxInd], 
				perfRateName = perfRateName,perfRateMaxList = summary[, perfRateMaxInd]))
}))

bestRest <- lapply(PMHTNT, function(pmhtnt){
			res <- FDA_results[[pmhtnt]]$perfRateMaxList
			names(res) <- FDA_results[[pmhtnt]][["perfRateName"]]
			return(res)})


allSummaries



finalRest 	<- vector("list", length(PMHTNT))

finalRest <- lapply(PMHTNT, function(pmhtnt){  
			mat <- matrix(NA, ncol = 5, nrow = length(mwPosPercentages) * length(xreg_versions) * length(versions))
			colnames(mat) <- c("MW", "PR", "AR", "X", "Y")
			finalRest[[pmhtnt]] <- mat
			return(finalRest[[pmhtnt]])})
names(finalRest) <- PMHTNT

	# head(finalRest[[pmhtnt]])
	
for( pmhtnt in PMHTNT){ # pmhtnt <- "POS_HT"
ct <- 0
	for(ver in versions){ # ver <- versions[1]
		for(xreg_ver in xreg_versions){# xreg_ver <- xreg_versions[1]
			
			print(paste0(ver, ".", xreg_ver))
			
			percCounter <- 1
			for(perc in mwPosPercentages){ # perc <- mwPosPercentages[66]
				
				x <- as.numeric(substr(xreg_ver, 2,5))
				y <- as.numeric(substr(ver, 2,5))
				
				listName 	<- paste0("v",y, ".x",x,".",perc)
				print(paste0(pmhtnt,listName))

				finalRest[[pmhtnt]][ (ct * length(mwPosPercentages))+ percCounter , "MW"] <- perc

				finalRest[[pmhtnt]][ (ct * length(mwPosPercentages))+ percCounter , "PR"] <- allSummaries[[listName]][pmhtnt, "perfRates"]
				finalRest[[pmhtnt]][ (ct * length(mwPosPercentages))+ percCounter , "AR"] <- allSummaries[[listName]][pmhtnt, "accRates"]
				finalRest[[pmhtnt]][ (ct * length(mwPosPercentages))+ percCounter , "X"] <- x
				finalRest[[pmhtnt]][ (ct * length(mwPosPercentages))+ percCounter , "Y"] <- y
				percCounter <- percCounter + 1
			}
		ct <- ct + 1 
		}
	}
}


ffResult <- lapply(PMHTNT, function(pmhtnt){
	df <- as.data.frame(finalRest[[pmhtnt]])
	df$YX <- paste0(finalRest[[pmhtnt]][ ,"Y"],"-", finalRest[[pmhtnt]][ ,"X"])
	
	breaks <- c(-0.0001, 0.5, 0.75, 0.9, 1) 
	labels <- c(paste0("<=",breaks[-1]))
	AR_Categories <- cut(finalRest[[pmhtnt]][ ,"AR"], breaks = breaks, labels = labels)

	df$AR_Categories <- AR_Categories
	return(df)
})
names(ffResult) <- PMHTNT

	# head(finalRest[[pmhtnt]])
for(pmhtnt in PMHTNT){
    print(pmhtnt)
	
	assign(x = paste0("perfPlot", pmhtnt),
	value = ggplot(	ffResult[[pmhtnt]], aes(x = MW, y = PR, colour = YX)) + 
	geom_line() + 
	theme_bw() + 
	scale_y_continuous(breaks = seq(0, 1.25, by = 0.25), limits = c(0, 1.25)) +
	scale_x_continuous(breaks = seq(0.35,1, by = 0.05)) +
	geom_hline(yintercept=1, color = "black", size=0.4) +
	geom_point(aes(shape = AR_Categories, size = AR_Categories)) + 
	scale_shape_manual(values=c(17, 18, 19, 15)) + 
	ylab(label="Performance Rate ") + 
	xlab("Percentages of future demanded Capacity") + 
	ggtitle(pmhtnt) + 
	theme(legend.position = "bottom")  + 
	guides(colour = guide_legend(nrow = 2)))
	
    readKey()
}


pdf(paste0("C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves\\", format(Sys.time(), format="%Y%m%d"), "perfPlot_all_vertical.pdf"), width = 17, height = 26)
grid.arrange(	perfPlotPOS_HT + theme(legend.position = "none"), 
				perfPlotPOS_NT + theme(legend.position = "none"), 
				perfPlotNEG_HT + theme(legend.position = "none"), 
				perfPlotNEG_NT, nrow=4, ncol = 1, heights = c(6, 6,6,8),
			  top=textGrob("Capacity Price in \u20ac/MW/w",gp=gpar(fontsize=20,font=1)))
dev.off()

?grid.arrange
##################################################
# Choose to plot 

FDA_results

ver 		<- "v3"
xreg_ver 	<- "x6"
perc 		<- "0.44"
allSummaries[[paste0(ver, ".", xreg_ver, ".",perc)]]
chosen 		<- fda_ver_result[[ver]][[xreg_ver]][[perc]]$fc

fda_ver_result[[ver]][[xreg_ver]][[perc]]$fc[[pmhtnt]]
#### plot methodology

fcPMHTNT    <- lapply(PMHTNT, function(pmhtnt) sapply(chosen[[pmhtnt]], function(fcs) fcs["fc"]))
names(fcPMHTNT) <- PMHTNT
maxPMHTNT   <- lapply(PMHTNT, function(pmhtnt) bdMax[[pmhtnt]][forecastInd+1])
names(maxPMHTNT) <- PMHTNT
gavgPMHTNT   <- lapply(PMHTNT, function(pmhtnt) bdQuan[[pmhtnt]][forecastInd+1, "gavg"])
names(gavgPMHTNT) <- PMHTNT
NotAccPMHTNT   <- lapply(PMHTNT, function(pmhtnt) sapply(chosen[[pmhtnt]], function(fcs) fcs["acc"]==0)) # sum( sapply(chosen[[pmhtnt]], function(fcs) fcs["acc"]) )
names(NotAccPMHTNT) <- PMHTNT # NotAccPMHTNT[[pmhtnt]]

for(pmhtnt in PMHTNT){
    print(pmhtnt)
	plot(forecastInd, fcPMHTNT[[pmhtnt]], col = "blue", type = "l", lwd = 2, ylim = c(0, max( maxPMHTNT[[pmhtnt]])))
    lines(forecastInd, gavgPMHTNT[[pmhtnt]], lty = 2, lwd = 2) 
    lines(forecastInd, maxPMHTNT[[pmhtnt]], lty = 1, col = "darkred", lwd = 2)
    points(x= forecastInd[NotAccPMHTNT[[pmhtnt]]], y= rep(0, times = length(forecastInd[NotAccPMHTNT[[pmhtnt]]])) , col = "red", pch = 16, cex =1)
    abline(v= forecastInd[NotAccPMHTNT[[pmhtnt]]], col = "red", lwd =0.5)
    abline(h=0, col ="black", lwd = 2)
    accAperfRate <- round(allSummaries[[paste0(ver, ".", xreg_ver, ".",perc)]][pmhtnt, ],4)*100
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
accAperfRate <- round(sumUp_FDA[pmhtnt, ],2)
legend(	"topleft", col = c("blue", "black", "darkred", NA), inset = 0.01, 
        legend = c("Forecast", paste0("MaxAcc (", accAperfRate[2], "%)"), paste0("Acc = -2 (", accAperfRate[1], "%)")),
        lwd=c(2,1.5,2, NA), lty = "solid", bg = "white")
abline(h=0, col ="darkgrey")





##############################
############################## 
##############################
# Prophet: 
load(file = paste0("R_Data/completePR_FC_List_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))
pr_ver_result

allSummaries <- lapply(pr_ver_result, function(vs) lapply(vs, function(xs) xs[["summary"]]))
 

perPMHTNT_Summary_AA <- vector("list", length(PMHTNT)) 
names(perPMHTNT_Summary_AA) <- PMHTNT

for(pmhtnt in PMHTNT){
	perPMHTNT_Summary_AA[[pmhtnt]]	<- matrix(NA, ncol = 2, nrow = length(xreg_versions) * length(versions))
	colnames(perPMHTNT_Summary_AA[[pmhtnt]]) <- c("accRates", "perfRates")
}
	


counter <- 1 
for(ver in versions){ # ver <- "v1"
	for(xreg_ver in xreg_versions){ # xreg_ver <- "x1"
		for(pmhtnt in PMHTNT){		
			perPMHTNT_Summary_AA[[pmhtnt]][counter, ] <- allSummaries[[ver]][[xreg_ver]][pmhtnt, ]	
		}
		counter <- counter + 1 
	}
} 


for(pmhtnt in PMHTNT){
	rownames(perPMHTNT_Summary_AA[[pmhtnt]]) <- c(t(outer(versions, xreg_versions, FUN=paste0)))
}

lapply(perPMHTNT_Summary_AA, function(summary) { # summary <- perPMHTNT_Summary_AA[[pmhtnt]]
	accRateMaxInd <- which.max(summary[,1])
	accRateName 	<- rownames(summary)[accRateMaxInd]
	perfRateMaxInd <- which.max(summary[,2])
	perfRateName 	<- rownames(summary)[perfRateMaxInd]
	return(list(accRateName = accRateName, accRateMaxList = summary[accRateMaxInd, ], 
				perfRateName = perfRateName,perfRateMaxList = summary[perfRateMaxInd, ]))
})


 
allSummaries[[1]]
ver <-allSummaries[[1]]

perPMHTNT_Summary <- lapply(PMHTNT, function(pmhtnt) sapply(allSummaries, function(ver) t(sapply(ver, function(x) x[pmhtnt, ]))))
names(perPMHTNT_Summary) <- PMHTNT

lapply(perPMHTNT_Summary, function(summary) { # summary <- perPMHTNT_Summary[[1]]
	accRateMaxInd <- which.max(summary[1, ])
	accRateName 	<- names(accRateMaxInd)
	perfRateMaxInd <- which.max(summary[2, ])
	perfRateName 	<- names(perfRateMaxInd)
	return(list(accRateName = accRateName, accRateMaxList = summary[, accRateMaxInd], 
				perfRateName = perfRateName,perfRateMaxList = summary[, perfRateMaxInd]))
})


###############################
# Auto Arima
load(file = paste0("R_Data/completeAA_FC_List_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))
allSummaries <- lapply(aa_ver_result, function(vs) lapply(vs, function(xs) xs[["summary"]]))
 

perPMHTNT_Summary_AA <- vector("list", length(PMHTNT)) 
names(perPMHTNT_Summary_AA) <- PMHTNT

for(pmhtnt in PMHTNT){
	perPMHTNT_Summary_AA[[pmhtnt]]	<- matrix(NA, ncol = 2, nrow = length(xreg_versions) * length(versions))
	colnames(perPMHTNT_Summary_AA[[pmhtnt]]) <- c("accRates", "perfRates")
}
	


counter <- 1 
for(ver in versions){ # ver <- "v1"
	for(xreg_ver in xreg_versions){ # xreg_ver <- "x1"
		for(pmhtnt in PMHTNT){		
			perPMHTNT_Summary_AA[[pmhtnt]][counter, ] <- allSummaries[[ver]][[xreg_ver]][pmhtnt, ]	
		}
		counter <- counter + 1 
	}
} 


for(pmhtnt in PMHTNT){
	rownames(perPMHTNT_Summary_AA[[pmhtnt]]) <- c(t(outer(versions, xreg_versions, FUN=paste0)))
}

lapply(perPMHTNT_Summary_AA, function(summary) { # summary <- perPMHTNT_Summary_AA[[pmhtnt]]
	accRateMaxInd <- which.max(summary[,1])
	accRateName 	<- rownames(summary)[accRateMaxInd]
	perfRateMaxInd <- which.max(summary[,2])
	perfRateName 	<- rownames(summary)[perfRateMaxInd]
	return(list(accRateName = accRateName, accRateMaxList = summary[accRateMaxInd, ], 
				perfRateName = perfRateName,perfRateMaxList = summary[perfRateMaxInd, ]))
})


 
allSummaries[[1]]
ver <-allSummaries[[1]]

perPMHTNT_Summary <- lapply(PMHTNT, function(pmhtnt) sapply(allSummaries, function(ver) t(sapply(ver, function(x) x[pmhtnt, ]))))
names(perPMHTNT_Summary) <- PMHTNT

lapply(perPMHTNT_Summary, function(summary) { # summary <- perPMHTNT_Summary[[1]]
	accRateMaxInd <- which.max(summary[1, ])
	accRateName 	<- names(accRateMaxInd)
	perfRateMaxInd <- which.max(summary[2, ])
	perfRateName 	<- names(perfRateMaxInd)
	return(list(accRateName = accRateName, accRateMaxList = summary[, accRateMaxInd], 
				perfRateName = perfRateName,perfRateMaxList = summary[, perfRateMaxInd]))
})


	