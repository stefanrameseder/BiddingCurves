##########################################################################################  
##########################################################################################
## Preprocessing of bcd data via monotSpline
## Depends on coneproj Version 1.11 (latest Version is >1.13)
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



for(pricetype in PriceTypes){ # pricetype <- PriceTypes[1]
	### Load raw data
	load(file = paste0("R_Data/",rla ,"_", pricetype ,"_bidcurves.RData"))

	str(bidcurves, max = 1)
	## bidcurves
	# $ dates : Date[1:304], format: "2011-06-27" "2011-07-04" ...
	 # $ POS_HT:List of 304
	  # .. [list output truncated]
	 # $ POS_NT:List of 304
	  # .. [list output truncated]
	 # $ NEG_HT:List of 304
	  # .. [list output truncated]
	 # $ NEG_NT:List of 304
	  # .. [list output truncated]

	### Test:
	#  monotSpline(curvedata = bidcurves[[ "POS_HT" ]][[1]], deriv = 0, lambda_opt = TRUE, increasing = TRUE, trace = FALSE)
	  
	log_bidcurves_smoothed <- list()
	  
	### Parallel Setup
	cl       	          <- makeCluster(detectCores()) # for 6 processors
	## load required libraries
	clusterEvalQ(cl, {	library(quadprog) 
						library("orthogonalsplinebasis")
						library("coneproj") # for coneA
						library("MASS") # for Null
	})

	clusterExport(cl, varlist=c(loadedFunctions, "evaluate", "SplineBasis","GramMatrix", "bidcurves"), envir=environment()) # send necessary objects of global environments to all Clusters
	# clusterEvalQ(cl, sessionInfo())

	for(pm in PM){
	  for(htnt in HTNT){ # pmhtnt <- "POS_HT"
		print(pmhtnt                                    <- paste0(pm, "_", htnt))
		# Transform all values smaller <1 -> 1
		log_bidcurves                             <- lapply(bidcurves[[ pmhtnt ]], function(xy){ # xy <- bidcurves[[ pmhtnt ]][[304]] 
		                                                        y <- xy[ ,2]
		                                                        y[y<1] <- 1
		                                                        return(xy = cbind(xy[ ,1], log(y)))})
		clusterEvalQ(cl, "log_bidcurves")
		
		print(system.time(log_bidcurves_smoothed[[pmhtnt]]  <- parLapply(cl, log_bidcurves, monotSpline, # with options
															deriv = 0, lambda_opt = TRUE, increasing = TRUE, 
															trace = FALSE)))
	  }
	}

	stopCluster(cl) 

	### Save smoothed data
	save(x = log_bidcurves_smoothed, file = paste0("R_Data/",rla ,"_", pricetype ,"_log_bidcurves_smoothed.RData"))
}
