##########################################################################################  
##########################################################################################
## Supplement for   "Bidding Curve Dynamics (BCD)"						            
## Forecast of bidding curves
## FDA Approach based forecast
## Author:          Stefan Rameseder /                               
##                  stefan.rameseder@ur.de /                                                
rm(list = ls())
user 	<- "stefan"
# Automized working directory choice 
if(user == "stefan"){
    if(Sys.info()[1]=="Darwin") {
        setwd("/Users/stefanrameseder/Google Drive/BCD")
        source("R_Code/bcd_emp_2_setupData.r")
    } else if(Sys.info()[1]=="Windows") {
		setwd("X:\\BiddingCurves") # list.files(); getwd()
		source("bcd_emp_2_setupData.r")
	}
}


#####################################################################################
## Data is now loaded
# Complete datasets
str(biddata, max = 1)
str(biddata[[1]], max = 1)
# Discrete step curves
str(bidcurves, max = 1) 
# Functional data set
str(bidcurves_smoothed, max = 1)       
dates
# LP Quantiles 
bdQuan
# LP Max Values 
bdMax
# MW Positions (non-equidistant)
bdMW
# Maximal demanded MW (univariate)
bdDem


overall_price <- vector("list", length(PMHTNT)) 
names(overall_price) <- PMHTNT
for(pmhtnt in PMHTNT){
	overall_price[[pmhtnt]] <- sum(bdQuan[[pmhtnt]]$gavg * bdDem[[pmhtnt]])
}
sum(unlist(overall_price))/10^6

##########################################################################################
## Chose Makroparameters:
T           		<- length(dates)
H					<- 1 		# Forecast Horizon
B					<- 100     	# Backtesting Horizon: how many forecasts will be compared
forecastInd			<- (T-B):(T-1) # These are the indices t for which we do forecasts t+1|t and compare them with real values in t+1
md_dis 				<- seq(0, 2500, length.out = 501)

# MINLP Parameters 
AL 					<- 199 		# Auction loss
rho 				<- auctionLossToRho(AL)
threshold			<- 5 		# If bid is lower than threshold, bid 0
Bootstrap			<- 1000 	# Number of Bootstrap Replications
K 					<- "all"	# Estimation Horizon; "all" = means complete time series starting at t = 3, a number means t-k as starting point
temp        		<- 1000     # Temperature of generalized simulated annealing
maxit       		<- 1000     # Maximum number of itereations for simulated annealing
model				<- "modMINLP"
ver					<- "v1"

# FDA Parameters 
maxBasisLength 		<- 51		# The Basis Selection Criterion in BIC
diffToMaxFutDemand 	<- 100 		# When demand in T+1 is x, x-100 MW will be focused as position on the transferred mean 
basisSel			<- "Med" 	# The Basis Selection Criterion in BIC 
basis_choice		<- "fourier"# The basis where the functions are projected onto
mwPosPercentages	<- c(seq(0.35, 1, by = 0.01)) # Percentages of MW Positions

model				<- "modMINLP"
versions 			<- c("v1", "v2", "v3", "v4", "v5", "v6") # for auto arima, prophet and fda 
xreg_versions 		<- c("x1", "x2", "x3", "x4", "x5", "x6", "x7") # for auto arima, prophet and fda 


# Find components on "fully observed" domain [MW, 1830MW) pmhtnt = "POS_HT"
if(pmhtnt == "POS_HT" || pmhtnt == "POS_NT"){
	minDemand <- 1900
} else if (pmhtnt == "NEG_HT" || pmhtnt == "NEG_NT") {
	minDemand <- 1830
}
d_max       <- which.min(md_dis < minDemand)
d_min       <- which.max(md_dis >= 0)


print(paste0("Length of Forecasts: ", length(forecastInd)))
print(paste0("#MW Percentages:", length(mwPosPercentages)))


#source("bcd_emp_3_b_1_forecast_FDA.r")
#source("bcd_emp_3_c_forecast_Prophet.r")
#source("bcd_emp_3_d_forecast_AutoArima.r")