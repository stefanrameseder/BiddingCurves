## ###########################################
rm(list=ls())

user     	<- "stefan" 		# User Choice for Working Directory

if(user == "stefan"){
    if(Sys.info()[1]=="Darwin") {
        setwd("/Users/stefanrameseder/Google Drive/BCD/R_Code")
    } else {
        setwd("C:/Users/LocalAdmin/Google Drive/BCD/R_Code")
    }
}


## Source Simulation DGP Data for DGP_name 
# list.files()
source("bcd_setDataAndLibraries.R")

##########################################################################################
## Source Data
# For Data Import in the file "data" a list containing two dataframes
load("R_Data/forecastMiNLP_data.RData") 
str(data)
length(data)
AL 			<- 15 		# Auction loss
threshold	<- 0.2 		# If bid is lower than threshold, bid 0
H			<- 1 		# Forecast Horizon
B			<- 100      # Estimation Horizon

result_l	 	<- vector("list", length(data)); lapply(result_l, function(entry) entry <- vector("list", length(data)))
names(result_l) <- c("POS", "NEG")
### Small differences from before: 
# 1. One Bidfunction
# 2. One Optimization

for( entry in 1:length(data)){ # entry <- 1
	
	# Maximum of lags and number of parameters needed for bidding function
	goBack		<- attributes(bid_function1)[[1]]  
	nPar		<- attributes(bid_function1)[[2]]
	
	# The dataset for entry "data[[entry]]"
	y			<- data[[entry]][ , c("MinimumCapacityPrice", "AverageCapacityPrice", "MaximumCapacityPrice")]
	T 			<- dim(y)[1]
	y_max 		<- y[ ,"MaximumCapacityPrice"]
	names(y_max)<- data[[entry]][, "Date"]
	
	# Starting Values
	par 		<- runif(nPar, -1, 1)
	low			<- rep(-5, times = nPar)
	up			<- rep(5, times = nPar)
	
	
	## Optimization via SANN in optim
	print(system.time(
	sol			<- optim(		par=par,
								fn = calcTotalLoss,  AL = AL, y = y, y_max = y_max,
								T = T, bf = bidFunction1, goBack = goBack, 
								lower=low, upper=up, method = "L-BFGS-B",
								control=list(maxit=3000))
	))
	## Forecast via SANN
	add_y 		<- matrix(as.matrix(y[T, ]), nrow = H, ncol = dim(y)[2], byrow = TRUE)
	y_fc  		<- rbind(as.matrix(y[ , ]), add_y)
	fc 			<- bid_function1(par = sol$par, y = y_fc, goBack = goBack)
	fc[fc < threshold] <- 0
	print(list(SANN_model = sol$par, SANN_forecast =  fc[(T+1-goBack):(T-goBack+H)]))
}	

	
##########################################################################################
## Model Evaluation
## Each result_l is a list of two entries "pm", each a list with 2 entries "htnt" and should be used like
summarizeForecast(result_l, data)

