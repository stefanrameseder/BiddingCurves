##########################################################################################  
##########################################################################################
## Supplement for   "Bidding Curve Dynamics (BCD)"						            
## Empirical Application 
## II. Setup all necessary data for bidding forecasts
## Author:          Stefan Rameseder /                               
##                  stefan.rameseder@ur.de /                                                
rm(list = ls())

# User Choice (for using Git for different users and using Dropbox/Owncloud/different pcs for single user)
user            <- "stefan" 
pricetype 		<- "LP"
# Automized working directory choice 
if(user == "stefan"){
    if(Sys.info()[1]=="Darwin") {
        setwd("/Users/stefanrameseder/Google Drive/BCD/R_Code")
        source("/Users/stefanrameseder/Google Drive/BCD/R_Code/bcd_aaa_setDataAndLibraries.r")
        #source("bid_sim_0_setDGPsAndLibraries.r")
        
		## Load Sanitized pricetype Bids (biddata), bidcurves tuples (bidcurves) and the monotone P Spline preprocessed curves (bidcurves_smoothed)
        load(paste0("/Users/stefanrameseder/Google Drive/BCD/R_Code/R_Data/SRL_",pricetype,"_biddata.RData"))
        load(paste0("/Users/stefanrameseder/Google Drive/BCD/R_Code/R_Data/SRL_",pricetype,"_bidcurves.RData"))
        load(paste0("/Users/stefanrameseder/Google Drive/BCD/R_Code/R_Data/SRL_",pricetype,"_bidcurves_smoothed.RData"))
        load(paste0("R_Data/bid_emp_", pricetype, "_evalMonPSplines.RData"))# variable: eval_psplines
        #load("R_Data/bias_emp_ftcEstDifferentBasisLength.RData")
		#load("R_Data/bias_emp_combinedFtcEstDifferentBasisLength.RData")
    } else if(Sys.info()[1]=="Windows") {
        setwd("X:\\BiddingCurves")
        source("bcd_aaa_setDataAndLibraries.r")
        #source("bid_sim_0_setDGPsAndLibraries.r")
        
		## Load Sanitized pricetype Bids (biddata), bidcurves tuples (bidcurves) and the monotone P Spline preprocessed curves (bidcurves_smoothed)
        load(paste0("R_Data\\SRL_",pricetype,"_biddata.RData"))
        load(paste0("R_Data\\SRL_",pricetype,"_bidcurves.RData"))
        load(paste0("R_Data\\SRL_",pricetype,"_bidcurves_smoothed.RData"))
		}
} 


#####################################################################################
str(biddata[[1]], max = 1)
# List of 22
 # $ quantiles  :'data.frame':    304 obs. of  10 variables:
 # $ bids       :'data.frame':    139 obs. of  608 variables:
  # .. [list output truncated]
 # $ dates      : Date[1:304], format: "2011-06-27" "2011-07-04" ...
 # $ mws        : int [1:139, 1:304] 10 20 25 50 75 90 100 110 125 135 ...
 # $ price      : num [1:139, 1:304] 450 460 476 500 525 ...
 # $ mws_zero   : int [1:139, 1:304] 0 10 15 40 65 80 90 100 115 125 ...
 # $ bidSizes   : num [1:139, 1:304] 10 10 5 25 25 15 10 10 15 10 ...
 # $ bidSizesc  : num [1:140, 1:304] 0 10 10 5 25 25 15 10 10 15 ...
 # $ nBids      : int [1:304] 60 57 68 71 74 41 73 46 68 72 ...
 # $ apLoss     : num 380
 # $ price_hour : num [1:139, 1:304] 7.5 7.7 7.9 8.3 8.8 9.1 9.2 9.5 9.6 10 ...
 # $ gavgh      : num [1:304] 14 14.3 14.1 13.8 13.2 ...
 # $ maxh       : num [1:304] 17.3 15.6 15.1 14.4 14 ...
 # $ dates_mw   : Date[1:29], format: "2011-06-27" "2011-10-03" ...
 # $ demCap     :List of 5
 # $ mwsc       : num [1:140, 1:304] 0 10 20 25 50 75 90 100 110 125 ...
 # $ price_clean: num [1:140, 1:304] 450 450 460 476 500 ...
 # $ maxMW      : num [1:304] 2101 2101 2101 2101 2101 ...
 # $ holidays   :'data.frame':    304 obs. of  2 variables:
 # $ mwsc_zero  : num [1:140, 1:304] 0 10 20 25 50 75 90 100 110 125 ...
 # $ binGrd     : num [1:2500] 1 2 3 4 5 6 7 8 9 10 ...
 # $ binMat     : num [1:2500, 1:304] 450 450 450 450 450 450 450 450 450 450 ...

str(bidcurves[[2]], max = 1) 
# Save rawdata in a list of length T with (x_i, y_i) pairs 
 
 
str(bidcurves_smoothed, max = 1)
### - SRL_LP/AP_bidcurves_smoothed.RData
# For each curve 
# - constrained.coefs: constrained coefficients for cubic B-Splines
# - unconstrained.coefs: unconstrained coefficients for cubic B-Splines
# - x.basis: Basis evaluated at x
# - eval_x.basis: Basis evaluated at eval_x
# - unconstrained.curve: predicted unconstrained values at eval_x
# - constrained.curve: predicted constrained values at eval_x
# - knots: knot choice sequence
# - df: edf = effective degrees of freedom
# - lambda: optimal lambda or chosen lambda for penalizing second derivative
 
str(biddata[[1]], max = 1)
# We are interested in LPS; 
# 1. Quantiles;
# 2. Max
# 3. Demanded Capacity
# 4. 
names(biddata[[1]]$demCap)
dates 			<- biddata[[1]]$dates

# LP Quantiles 
(bdQuan 			<- lapply(PMHTNT, function(pmhtnt) biddata[[pmhtnt]]$quantiles))
names(bdQuan) 	<- PMHTNT

# LP Max Values 
(bdMax 			<- lapply(PMHTNT, function(pmhtnt) biddata[[pmhtnt]]$quantiles$max))
names(bdMax) 	<- PMHTNT

# MW Positions (non-equidistant)
(bdMW 			<- lapply(PMHTNT, function(pmhtnt) biddata[[pmhtnt]]$mwsc))
names(bdMW) 	<- PMHTNT

# Maximal demanded MW (univariate)
(bdDem 			<- lapply(PMHTNT, function(pmhtnt) apply(bdMW[[pmhtnt]], 2, max, na.rm = TRUE)))
names(bdDem) 	<- PMHTNT




	