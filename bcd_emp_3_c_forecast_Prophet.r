

pr_xreg_result 		<- vector("list", length(xreg_versions)) 
names(pr_xreg_result) <- xreg_versions
pr_xreg_result 		<- lapply(pr_xreg_result, function(x) vector("list", length(mwPosPercentages)))  

pr_ver_result 		<- vector("list", length(versions)) 
names(pr_ver_result) <- versions
pr_ver_result 		<- lapply(pr_ver_result, function(x) vector("list", length(xreg_versions)))  



# As for loop 
system.time(for(ver in versions){ # ver <- "v1"
	for(xreg_ver in xreg_versions){ # xreg_ver <- "x6"		
			
	 # mwPosPerc <- 0.35 
		print(paste0(" Ver: ", ver, " XREG: ", xreg_ver))
		pmhtntpr <- lapply(PMHTNT, function(pmhtnt) {
					resList <- lapply(forecastInd, forecastProphet, # ind <- 204
															pmhtnt, # pmhtnt <- "POS_HT"
															dates = dates, 
															biddata = biddata, 
															ver 	= ver, 
															xreg_ver = xreg_ver,
															md_dis = md_dis
															) # 0.5
					# Each forecast is t+1|t is named by t+1
					names(resList) <- dates[forecastInd+1]
					return(resList)
				})
		names(pmhtntpr) <- PMHTNT
		sumUp_pr <- summarizeFC(pmhtntpr, B = length(forecastInd), bdQuan, PMHTNT)
		print(sumUp_pr)

		
		
		pr_xreg_result[[xreg_ver]] <- list(
														summary = sumUp_pr, 
														fc = pmhtntpr)
	}
	pr_ver_result[[ver]] <- pr_xreg_result
})

save(pr_ver_result, file = paste0("R_Data/completePR_FC_List_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))
