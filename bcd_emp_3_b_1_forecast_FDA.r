
evalBidcurves <- lapply(PMHTNT, function(pmhtnt){
	pmhtntList <- lapply(1:length(dates), function(t){
		return(list(fct = bidcurves_smoothed[[pmhtnt]][[t]]$constrained.curve(md_dis, deriv = 0),
					der = bidcurves_smoothed[[pmhtnt]][[t]]$constrained.curve(md_dis, deriv = 1))) 
	})
	return(pmhtntList)
})
names(evalBidcurves) <- PMHTNT


# plot(x = md_dis, y = evalBidcurves[[pmhtnt]][[1]]$fct, type = "l")
# plot(x = md_dis, y = evalBidcurves[[pmhtnt]][[1]]$der)


# matplot(x = md_dis, y=sapply(evalBidcurves[[pmhtnt]], function(x) x[[1]]), # dim(combinedNEG)
        # col = PartiallyFD:::addAlpha("black", 0.2), type = "l", lwd = 1,
        # ylab = "", xaxt = "n", yaxt = "n", lty = "solid")

	
load(file = paste0("R_Data/ftc_Estimates_",T-B, "_", T-1, ".RData"))


#########################################################################################################################
# As for loop
fda_result <- vector("list", length(mwPosPercentages)) 
names(fda_result) <- mwPosPercentages
fda_result 	<- lapply(fda_result, function(x) vector("list", 2))  

fda_xreg_result <- vector("list", length(xreg_versions)) 
names(fda_xreg_result) <- xreg_versions
fda_xreg_result 	<- lapply(fda_xreg_result, function(x) vector("list", length(mwPosPercentages)))  

fda_ver_result <- vector("list", length(versions)) 
names(fda_ver_result) <- versions
fda_ver_result 	<- lapply(fda_ver_result, function(x) vector("list", length(xreg_versions)))  

 
system.time(for(ver in versions){ # ver <- "v2"
	for(xreg_ver in xreg_versions){ # xreg_ver <- "x3"		
			
		for(mwPosPerc in mwPosPercentages){ # mwPosPerc <- 0.3
			print(paste0("MW: ", mwPosPerc, " Ver: ", ver, " XREG: ", xreg_ver))
			pmhtntFDA <- lapply(PMHTNT, function(pmhtnt) {
						resList <- lapply(forecastInd, forecastMaxFDA, # ind <- 204
																pmhtnt, # pmhtnt <- "POS_HT"
																dates = dates, 
																biddata = biddata, 
																xreg_ver = xreg_ver,
																mwPosPerc = mwPosPerc,
																ver 	= ver, 
																ftc_Estimates_pmhtnt = ftc_Estimates[[pmhtnt]], # str(ftc_Estimates, max = 1)
																md_dis = md_dis
																) # 0.5
						# Each forecast is t+1|t is named by t+1
						names(resList) <- dates[forecastInd+1]
						return(resList)
					})
			names(pmhtntFDA) <- PMHTNT
			sumUp_FDA <- summarizeFC(pmhtntFDA, B = length(forecastInd), bdQuan, PMHTNT)
			#print(sumUp_FDA)
			
			# Generate empty list 
			fda_result[[as.character(mwPosPerc)]] <- list(
															summary = sumUp_FDA, 
															fc = pmhtntFDA)
		}
		
		fda_xreg_result[[xreg_ver]] <- fda_result
	}
	fda_ver_result[[ver]] <- fda_xreg_result
})
save(fda_ver_result, file = paste0("R_Data/completeFDA_ver_result_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))

completeFDA_FC_List <- do.call(c, unlist(fda_ver_result, recursive=FALSE))

save(completeFDA_FC_List, file = paste0("R_Data/completeFDA_FC_List_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))

