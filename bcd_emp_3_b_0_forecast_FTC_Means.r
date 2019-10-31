

evalBidcurves <- lapply(PMHTNT, function(pmhtnt){
	pmhtntList <- lapply(1:length(dates), function(t){
		return(list(fct = bidcurves_smoothed[[pmhtnt]][[t]]$constrained.curve(md_dis, deriv = 0),
					der = bidcurves_smoothed[[pmhtnt]][[t]]$constrained.curve(md_dis, deriv = 1))) 
	})
	return(pmhtntList)
})
names(evalBidcurves) <- PMHTNT


plot(x = md_dis, y = evalBidcurves[[pmhtnt]][[1]]$fct, type = "l")
plot(x = md_dis, y = evalBidcurves[[pmhtnt]][[1]]$der)

data(log_partObsBidcurves)
attach(log_partObsBidcurves)

matplot(x = md_dis, y=sapply(evalBidcurves[[pmhtnt]], function(x) x[[1]]), # dim(combinedNEG)
        col = PartiallyFD:::addAlpha("black", 0.2), type = "l", lwd = 1,
        ylab = "", xaxt = "n", yaxt = "n", lty = "solid")


lapply(bdDem, function(x) c(min = min(x), max = max(x)))

# Find components on "fully observed" domain [MW, 1830MW)
if(pmhtnt == "POS_HT" ||pmhtnt == "POS_NT"){
	minDemand <- 1900
} else if (pmhtnt == "NEG_HT" || pmhtnt == "NEG_NT") {
	minDemand <- 1830
}

d_max       <- which.min(md_dis < minDemand)
d_min       <- which.max(md_dis >= 0)

	
ftc_Estimates <- vector("list", length(PMHTNT)) 
names(ftc_Estimates) <- PMHTNT	

# Estimate FTC Means for different sample sizes 
for(pmhtnt in PMHTNT){
	print(pmhtnt)
	
	cl       	          <- makeCluster(detectCores()) # for 6 processors
	## load required libraries
	clusterEvalQ(cl, {	library("PartiallyFD") })

	clusterExport(cl, varlist=c(loadedFunctions, "est_FTC_Mean_given_ind", "pmhtnt","evalBidcurves", "md_dis", "basis_choice", "maxBasisLength", "basisSel", "B"), envir=environment()) # send necessary objects of global environments to all Clusters
	# clusterEvalQ(cl, sessionInfo())

	print(system.time(ftc_Estimates[[pmhtnt]] <- parLapply(cl, forecastInd, est_FTC_Mean_given_ind, pmhtnt)))
	
	stopCluster(cl) 
	
	# A list of T-1 - T-B ftc Mean functions where the function t is estimated on observations up to t, i.e., the first one on functions up to T-B = 204 

}

save(x = ftc_Estimates, file = paste0("R_Data/ftc_Estimates_",T-B, "_", T-1, ".RData"))



	