aa_xreg_result <- vector("list", length(xreg_versions)) 
names(aa_xreg_result) <- xreg_versions
aa_xreg_result 	<- lapply(aa_xreg_result, function(x) vector("list", length(mwPosPercentages)))  

aa_ver_result <- vector("list", length(versions)) 
names(aa_ver_result) <- versions
aa_ver_result 	<- lapply(aa_ver_result, function(x) vector("list", length(xreg_versions)))  


# As for loop 
system.time(for(ver in versions){ # ver <- "v2"
	for(xreg_ver in xreg_versions){ # xreg_ver <- "x3"		
			
	 # mwPosPerc <- 0.35 
		print(paste0(" Ver: ", ver, " XREG: ", xreg_ver))
		pmhtntAA <- lapply(PMHTNT, function(pmhtnt) {
					resList <- lapply(forecastInd, forecastAutoArima, # ind <- 204
															pmhtnt, # pmhtnt <- "NEG_HT"
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
		names(pmhtntAA) <- PMHTNT
		sumUp_AA <- summarizeFC(pmhtntAA, B = length(forecastInd), bdQuan, PMHTNT)
		print(sumUp_AA)

		
		
		aa_xreg_result[[xreg_ver]] <- list(
														summary = sumUp_AA, 
														fc = pmhtntAA)
	}
	aa_ver_result[[ver]] <- aa_xreg_result
})

save(aa_ver_result, file = paste0("R_Data/completeAA_FC_List_id",length(forecastInd), "_perc", length(mwPosPercentages) ,".RData"))

