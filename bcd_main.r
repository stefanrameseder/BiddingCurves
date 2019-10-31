## ###########################################
## Simulations with KneipPossSarda Approach as Countermodel

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


## Load preprocessed data done via !!! bcd_preprocessing.r !!!
load("R_Data/bidcurves.RData") # load raw data
load("R_Data/bidcurves_smoothed.RData") # load preprocessed data via bcd_smoothing.r


##########################################################################################  
##########################################################################################
## Supplement for   "Bidding Curve Dynamics"						            
## 
## Author:          Stefan Rameseder 
##          
##                  stefan.rameseder@ur.de /                                                      
## Date:                                                                     
## Definitions:                                                        					
## matrices with "_m"                                                     			   	
## vectors with "_v"                                                        			
## arrays with "_ar"                                                        			
## lists with "_l"
## intervalls with "_i" 
## bc_fd with "_fd"
## global constants: capital letters        

pmhtnt              <- paste0(PM[1], "_", HTNT[1])

bid_data_l          <- bidcurves[[ pmhtnt ]] # a list of x_i, y_i tuples
bid_curves_l        <- bidcurves_smoothed[[pmhtnt]] # a list with smoothed values given by monotSpline


## Global variables
T                   <- length(bid_data_l) # number of curves
colours 	    	<- rainbow( T )

## Number of Factors 
K                   <- 2  		# number of principal components; determines undersmoothing as well               
K_possible          <- 1:5		# sequence of possible factors of which we choose
n_ev_points   	    <- 100 		# number of evaluation points for discretization 


## Other stuff
dates_i             <- as.Date(c("2011-06-27","2016-03-07"))
dates               <- seq(dates_i[1], dates_i[2], by = "week") 
mw_fixed            <- 1000 	# fixed MW Position for fitted plots; can be checked with other quantiles
example_curves      <- 100:105# which curves should be plotted?

## Domains 
com_dom_i           <- apply(matrix(unlist(lapply(bidcurves[[ pmhtnt ]], function(x) range(x[ ,1], na.rm = TRUE))), ncol = 2, byrow = TRUE), 2, min, na.rm = TRUE)  # range of intersection of all domains          			
max_dom_i           <- apply(matrix(unlist(lapply(bidcurves[[ pmhtnt ]], function(x) range(x[ ,1], na.rm = TRUE))), ncol = 2, byrow = TRUE), 2, max, na.rm = TRUE)  # range of intersection of all domains               

com_dom_grid_v	    <- seq( from = com_dom_i[1], to = com_dom_i[2], length.out = n_ev_points) # common domain discretization
max_dom_grid_v	    <- round(unique(c( com_dom_grid_v, seq(from = max(com_dom_grid_v), to = range(max_dom_i)[2], length.out=round(0.5 * n_ev_points, 0 )))), 0)														

## Prediction
oos_start			<- 100 # out of sample start

## Two Sample Tests
smpl1				<- 1:130
smpl2				<- 131:T

if(any(smpl1 %in% smpl2)) print("Beide Samples sind nicht disjunkt!")
################################################################
################################################################
## picture example curves 
# pdf(paste0("images/", format(Sys.time(), format="%Y%m%d"), "example_curves",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
par(mfrow = c( ceiling(length(example_curves)/2) , 2)) # two columns
plots   			<- vector("list", length = length(example_curves))

for(i in example_curves){
  smooth_function                   <- bid_curves_l[[ i ]]$constrained.curve
  plots[[i-min(example_curves)+1]]  <- plot_step_curve(bid_data_l,curve_index = i,smooth=TRUE,smooth_data = data.frame(x = bid_data_l[[ i ]][ ,1],y = smooth_function(bid_data_l[[ i ]][ ,1], deriv = 0)))
}
multiplot(plotlist = plots, cols=2)
dev.off()

################################################################
################################################################
## plot all smoothed curves at common domain


p               	<- ggplot()
for (i in 1:T) {# i = 1
  smooth_function   	<- bid_curves_l[[ i ]]$constrained.curve
  p                 	<- 	p +  geom_line(
							data = data.frame(x = com_dom_grid_v, y = smooth_function(com_dom_grid_v, deriv = 0)), 
							aes(x,y), colour=colours[i])
}
p

## first derivatives
p1 <- ggplot()
for (i in 1:T ) {
  smooth_function   	<- bid_curves_l[[ i ]]$constrained.curve
  p1                	<- p1 + geom_line(
								data = data.frame(x = com_dom_grid_v, y = smooth_function(com_dom_grid_v, deriv = 1)), 
								aes(x,y), colour=colours[i])
}
p1

## second derivatives
p2 <- ggplot()
for (i in 1:T ) {
  smooth_function   <- bid_curves_l[[ i ]]$constrained.curve
  p2                <- p2 +  geom_line(
      data = data.frame(x = com_dom_grid_v, y = smooth_function(com_dom_grid_v, deriv = 2)), 
      aes(x,y), colour=colours[i])
}
p2


bc_cdg_l 			<- eval_curves(bid_curves_l, grid = com_dom_grid_v, deriv = 0)
bc_cdg_m 			<- matrix(unlist(bc_cdg_l), nrow = T, ncol = length(com_dom_grid_v), byrow=TRUE)
## 1. Derivative
bcD1_cdg_l 			<- eval_curves(bid_curves_l, grid = com_dom_grid_v, deriv = 1)
bcD1_cdg_m 			<- matrix(unlist(bcD1_cdg_l), nrow = T, ncol = length(com_dom_grid_v), byrow=TRUE)
## 2 Derivative
bcD2_cdg_l  		<- eval_curves(bid_curves_l, grid = com_dom_grid_v, deriv = 2)
bcD2_cdg_m  		<- matrix(unlist(bcD2_cdg_l), nrow = T, ncol = length(com_dom_grid_v), byrow=TRUE)
################################################################
################################################################
## Different versions of mean functions! 
## 1. Version
## Take presmoothed data and calculate pointwise means at common grid
mean_v1           	<- colMeans(bc_cdg_m)
(p_with_mean        <- p + 	geom_line(data= data.frame(x = com_dom_grid_v, y=mean_v1),
							aes(x,y), colour="black" , size=3))

## 2. Version
## Take raw data and smooth non-parametrically at common domain
com_dom_obs       	<- lapply(bid_data_l, function(x) x[ x[, 1] < com_dom_i[2] , ]) # which values belong to common domain
y_values          	<- lapply(com_dom_obs, function(x) x[, 2])
x_values          	<- lapply(com_dom_obs, function(x) x[, 1])

local_linear_fit  	<- locfit(	unlist(y_values)~ lp( unlist(x_values), 
							   nn = 0.7, ## Nearest neighbor component of the smoothing parameter. Default value is 0.7, unless either h or adpen are provided, in which case the default is 0.
							   h = 0, adpen = 0, ## The constant component of the smoothing parameter. Default: 0. Penalty parameter for adaptive fitting.
							   deg = 2, ## Degree of polynomial to use.
							   acri = "none", ## Criterion for adaptive bandwidth selection.
							   scale = FALSE, style = "none" ) )
mean_v2           	<- predict(local_linear_fit	, newdata = com_dom_grid_v)


p_with_mean <- p_with_mean + geom_line(data= data.frame(x = com_dom_grid_v, y=mean_v2),aes(x,y), colour="yellow", linetype="dotted", size=3)


								   
## 3. Version
## Take raw data and use mon. penalized splines
mean_v3.m 			<- monotSpline(	cbind(x = unlist(x_values), y = unlist(y_values)),
                                               deriv = 0, lambda_opt = TRUE, increasing = TRUE, 
                                               trace = TRUE)

mean_v3 <- mean_v3.m$constrained.curve(com_dom_grid_v)
p_with_mean + geom_line(data= data.frame(x = com_dom_grid_v, y=mean_v3),aes(x,y), colour="red", linetype="dotted", size=3)

################################################################						
################################################################
## covariance function
# 1. direct calculation
bc_cdg_mean_m 		<- t( t(bc_cdg_m) - colMeans(bc_cdg_m)) ## mean substracted
covFunction   		<- cov(bc_cdg_mean_m)
persp( x=com_dom_grid_v, y=com_dom_grid_v, covFunction, theta = 10, phi = 10)
# 2. via fda
bc_fd        		<- Data2fd( argvals = com_dom_grid_v , y = t(bc_cdg_m))
covFuFda      		<- var.fd(bc_fd)

covFuEval     		<- eval.bifd(com_dom_grid_v, com_dom_grid_v, covFuFda) # evaluates the function at the grid
persp( x=com_dom_grid_v, y=com_dom_grid_v, covFuEval, theta = 10, phi = 10)

## covariance function of derivatives
bc_fd_D1       		<- Data2fd( argvals = com_dom_grid_v , y = t(bcD1_cdg_m))
covFuFda_D1    		<- var.fd(bc_fd_D1)
covFuEval_D1   		<- eval.bifd(com_dom_grid_v, com_dom_grid_v, covFuFda_D1) # evaluates the function at the grid

persp( x=com_dom_grid_v, y=com_dom_grid_v, covFuEval_D1, theta = 10, phi = 10)

par(mfrow = c(1,2)) # two columns; range(covFuEval_D1)
persp( x=com_dom_grid_v, y=com_dom_grid_v, covFuEval, theta = 10, phi = 10, nticks = 5)
persp( x=com_dom_grid_v, y=com_dom_grid_v, covFuEval_D1, theta = 10, phi = 10, zlim = c(0, 0.1), nticks = 5)
dev.off()

################################################################
################################################################
## PCA/Eigenanalysis
eigenDec      		<- eigen(covFunction)

### with fda package
bc_cdg_pca    		<- pca.fd( bc_fd , nharm=4 , centerfns=TRUE)

################################################################
################################################################
# first eigenfunctions and screeplot
plots      			<- list()
p3         	 		<- ggplot( data = data.frame( vals = eigenDec$values ,k=1:length( eigenDec$values)))
p3          		<- p3 + geom_point( aes(k,vals))
plots[[1]] 			<- p3
for (i in 1:4){
    p3            		<- ggplot( data = data.frame( x=com_dom_grid_v, y= eigenDec$vectors[,i]))
    p3            		<- p3 + geom_line( aes(x,y), colour="blue")
    p3            		<- p3 + geom_line(data = data.frame(x = com_dom_grid_v , y= eval.fd( com_dom_grid_v, bc_cdg_pca$harmonics)[,i]),
                                  aes(x,y), colour="red")
    plots[[i+1]] 		<- p3
}
multiplot(plotlist= plots)

################################################################
################################################################
# Comparison of eigenfunctions/eigenvalues

bc_cdg_pca$values[1:4]==eigenDec$values[1:4] ## why not??
bc_cdg_pca$values[1:4]/eigenDec$values[1:4]  ## almost same ratio
val1   				<- eigenDec$values
val2   				<- bc_cdg_pca$values
ratio1 				<- val1[1:4]/sum(val1)
ratio2 				<- val2[1:4]/sum(val2)
ratio1/ratio2

y1 					<- eval.fd( com_dom_grid_v, bc_cdg_pca$harmonics)
y2 					<- eigenDec$vectors[,1:4]
m  					<- mean(as.vector(y1/y2))
ym 					<- y1-y2*m
summary(ym)
plot(as.vector(ym))

################################################################
################################################################
## Depth of objects

## fda objects of curves and derivatives
bc_fd				<- fdata(Data2fd( argvals = com_dom_grid_v , y = t(bc_cdg_m)))
bc_D1_fd 			<- fdata(Data2fd( argvals = com_dom_grid_v , y = t(bcD1_cdg_m)))

bc_FMD    			<- depth.FM( bc_fd, trim=0)$dep # Fraiman and Muniz (FM) depth: The depth.FM function is based on integrated an univariate depth measure along the axis x. 
bc_MD     			<- depth.mode( bc_fd, trim=0)$dep # modal depth: The depth.mode function implements the modal depth that selects the curve most densely surrounded by others as the deepest one. By default, the distance is calculated using metric.lp function
bc_RP     			<- depth.RP( bc_fd, trim=0)$dep # random project (RP) depth: The depth.RP function implements a depth measure based on random projections 


plot(x=bc_MD, y=bc_RP) # difference for medium depth measueres
plot(x=bc_MD, y=bc_FMD) # difference for medium depth measueres
plot(x=bc_RP, y=bc_FMD) # similar depth values

## Comparison of implemented depths with selfmade depths
bc_MD_self  		<- modalDepth(bc_cdg_m , com_dom_grid_v) ## Modal Depth according to Febrero-Bande (2007)
bc_FM_self			<- fmDepth(bc_cdg_m , com_dom_grid_v) ##### Frairman/Muniz Depth, according to Febrero-Bande (2007)
bc_RP_self			<- rpDepth(curvedata = bc_cdg_m, curvedata_deriv = bcD1_cdg_m, com_dom_grid_v) ##### Frairman/Muniz Depth, according to Febrero-Bande (2007)


plot(x=bc_MD, y=bc_MD_self )
plot(x=bc_FMD, y=bc_FM_self)
plot(x=bc_FMD, y=bc_FM_self)

################################################################
################################################################
## "Deepest point" = median = function with maximum sum of all "inverse" distances to other functions
## "Deepest point" = median = function with maximum "nearness" to other functions

### Median of functions
# Median according to depth.mode = bc_MD
median  			<- which.max(bc_MD)
(pN     			<- p + geom_line(	data = data.frame(x = com_dom_grid_v, y = bc_cdg_m[median,]), 
										aes(x,y), colour="black", size=2))

# Median function according to median of first deriv via depth.mode
median_D1 			<- which.max(depth.mode( bc_D1_fd, trim=0)$dep)
pN + geom_line(data = data.frame(x = com_dom_grid_v, y = bc_cdg_m[median_D1, ]), aes(x,y), colour="blue", size=2) 

### Median of derivatives
# derivative of first median
(pN1    			<- p1 + geom_line(	data = data.frame(x = com_dom_grid_v, y = bcD1_cdg_m[median, ]), 
										aes(x,y), colour="black", size=2 ) + scale_y_continuous(limits=c(0,1)))

# Median derivative according to median of first deriv via depth.mode
median3 <- which.max(depth.mode( bc_fd, trim=0)$dep)
pN1 + geom_line(data = data.frame(x = com_dom_grid_v, y = bcD1_cdg_m[median_D1,]), 
      aes(x,y), colour="blue", size=2) 

# Summary: Median of derivatives and median of functions are not the same	  


################################################################
################################################################
## Outliers of functions
bc_out      		<- outliers.depth.trim(bc_fd)$outliers # applied to functional data object; uses trimmed data
(bc_outliers 		<- as.numeric(substr(bc_out,4,6))) # get indices
bc_outliers_FMD_self   	<- detectOutlierFD(bc_cdg_m , com_dom_grid_v, depthtype = "FMD" , trace=TRUE) # with selfmade function


## Outliers of derivatives
bc_D1_out 		<- outliers.depth.trim(bc_D1_fd)$outliers
(bc_D1_outliers		<- as.numeric(substr(bc_D1_out,4,6)))
bc_D1_outliers_self	<- detectOutlierFD(bcD1_cdg_m , com_dom_grid_v, depthtype = "FMD" , trace=TRUE)

## plot functions with both outliers
pOut <- p

for (out in bc_D1_outliers) {
    pOut 				<- pOut + geom_line(data=data.frame(x = com_dom_grid_v, y = bc_cdg_m[out, ]), 
											aes(x,y), colour="black", linetype = "dashed", size=1.5)
}
for (out in bc_outliers) {
    pOut 				<- pOut + geom_line(data = data.frame(x = com_dom_grid_v, y = bc_cdg_m[out, ]), 
											aes(x,y), colour="darkgrey", size=1.5)
}
pOut

## plot derivatives with both outliers
pOut_D1 <- p1
for (out in bc_outliers) {
    pOut_D1 				<- pOut_D1 + geom_line(data = data.frame(x = com_dom_grid_v, y = bcD1_cdg_m[out, ]), 
											aes(x,y), colour="darkgrey", size=1.5)
}

for (out in bc_D1_outliers) {
    pOut_D1 <- pOut_D1 + geom_line(data=data.frame(x = com_dom_grid_v, y = bcD1_cdg_m[out, ]), 
                             aes(x,y), colour="black", linetype="dashed", size=1.5)
}
pOut_D1


if(length(bc_outliers) < length(bc_D1_outliers)) {
	print("Mehr Ausreißer bei den Ableitungen")
} else{
	print("Mehr Ausreißer bei den Funktionen")
}	
bc_outliers[bc_outliers %in% bc_D1_outliers]
bc_D1_outliers[bc_D1_outliers %in% bc_outliers]

## Functional Boxplots:
fbplot( fit = t(bc_cdg_m), method = 'MBD')
fbplot( fit = t(bcD1_cdg_m), method = 'MBD')



################################################################
################################################################
## Phase Plane Plots = Qualitative technique for differential equations

p2 <- ggplot()
for (i in 1:T ) {
  p2 <- p2 +  geom_path(data = data.frame(velocity = bc_cdg_m[i,], acceleration = bcD1_cdg_m[i,]), 
      aes(velocity,acceleration), colour=colours[i])
}
p2

p3 <- ggplot()
for (i in 1:T ) {
  p3 <- p3 +  geom_path(data = data.frame(velocity = bcD1_cdg_m[i,], acceleration = bcD2_cdg_m[i,]), 
      aes(velocity,acceleration), colour=colours[i])
}
p3




################################################################
################################################################
## Functional Autoregression  FAR(1) 
## make an fdata object
## bc_cdg_m, p = dim(bc_cdg_m)[2] , dates=bidcurves$dates

(bc_cdg_fdata 		<- as.fdata(t(bc_cdg_m) , p=dim(bc_cdg_m)[2]))
(bcD1_cdg_fdata 	<- as.fdata(t(bc_cdg_m) , p=dim(bcD1_cdg_m)[2]))
plot(bc_cdg_fdata , whole=TRUE)

bc_cdg_FAR  		<- far(bc_cdg_fdata)
bcD1_cdg_FAR  		<- far(bcD1_cdg_fdata)


## out of sample prediction
for (i in oos_start:(T-1)) { # i <- 100
    fd.test  			<- as.fdata( t(bc_cdg_m[1:i,]) , p=100 )
    new.data 			<- as.fdata( as.vector(bc_cdg_m[i,]) , p=100 ) 
    est.far  			<- far(fd.test)
    predict  			<- as.vector( predict.far(est.far, newdata=new.data)[[1]] )
    plot(x=com_dom_grid_v , y=predict , type="l" , ylim=c(0,1200) )
    lines(x=com_dom_grid_v , y=bc_cdg_m[i+1,] , type="l" , col="green")
    legend("topleft" , col=c("black", "green") , legend=c("estimate" , "real" ) , lwd=2 )
    readkey()
}

## FAR(1) with derivatives
for (i in oos_start:(T-1)) { # i <- 100
    fd.test  			<- as.fdata( t(bcD1_cdg_m[1:i,]) , p=100 )
    new.data 			<- as.fdata( as.vector(bcD1_cdg_m[i,]) , p=100 ) 
    est.far  			<- far(fd.test)
    predict  			<- as.vector( predict.far(est.far, newdata=new.data)[[1]] )
    plot(x=com_dom_grid_v , y=predict , type="l" , ylim=c(0,6) )
    lines(x=com_dom_grid_v , y=bcD1_cdg_m[i+1,] , type="l" , col="green")
    legend("topleft" , col=c("black", "green") , legend=c("estimate" , "real" ) , lwd=2 )
    readkey()
}



################################################################
################################################################
## Tests
################################################################
################################################################

################################################################
## Equity of Mean for two samples
## Cf. Horvath/Kokoszka: Chapter 5
## split up data into two parts
					
bc1_m     			<- bc_cdg_m[smpl1, ]
bc2_m     			<- bc_cdg_m[smpl2, ]
N         			<- dim(bc1_m)[1]
M         			<- dim(bc2_m)[1]


mean1     			<- colMeans(bc1_m)
mean2     			<- colMeans(bc2_m)

diff      			<- mean1 - mean2

## Method 1: Simulate limiting distribution of U_nm
## compute U_nm
U         			<- integrate_xy( x = com_dom_grid_v , y = diff^2 )[n_ev_points]

theta     			<- N /( N + M )


c1        			<- cov(bc1_m)
c2        			<- cov(bc2_m)
z         			<- (1-theta) * c1 + theta * c2

eDec      			<- eigen(z)
val       			<- eDec$values
vecs      			<- eDec$vectors

## simulate 1000 values for limit distribution
R      				<- 10000
lim    				<- function(i) val %*% rnorm(100)^2
lims   				<- unlist(lapply(1:R, lim))
limOrd 				<- sort(lims)

hist(limOrd, n=100)

limitU 				<- limOrd[ R*0.95]

U > limitU # does not reject H0


## Method 2: take d leading eigenvals and functions
d         			<- 5
sum(val[2:100]-val[1:99]>0)  # (already decreasing)

sum(val[1:5]) / sum(val) # they explain 99.96 %
tau_v     			<- val[1:d]
psi_v     			<- vecs[,1:d]

A_v       			<- t( psi_v) %*% diff

T1        			<- N*M/(N+M) * sum( A_v^2/tau_v)
(pval      			<- pchisq(T1 , df=d , lower.tail = FALSE)) # no reject


T2        			<- N*M/(N+M) * sum(A_v^2)
T2 > limitU # no reject

plot( x= com_dom_grid_v , y = mean1 , type="l" , ylim=range(bc_cdg_m))
lines(x= com_dom_grid_v , y = mean2 , col="green")


### TO DO for Philipp

# 1. Binomial likelihood
# a) Simulate 1000 observations of a binomial random variable with p = 0.6
# b) Write down the loglikelihood
# c) Optimize the loglikelihood numerically with the function "optim"
# d) Write a small simulation with which you can plot the number of observations n = seq(50, 1000, by = 50) = x axis vs. your above estimated p s for each sample

# 2. Bivariate Normal
# a) Simulate 1000 observations of a bivariate random variable with zero mean and Sigma = matrix(c(1, 0.9, 0.9, 1), ncol = 2)
# ?mvrnorm
# b) Write down the loglikelihood
# c) Optimize the loglikelihood numerically with the function "optim"
# d) Plot the two dimensional density, i.e. get a three dimensional graph. 

# 3. Christmas influence: Do the curves above around christmas behave differently than the rest? 
# !! Test it for pm = POS AND pm = NEG AND htnt = HT AND htnt = NT !!
# Start a new .r-file and think about the function you would like to write. 
# a) Get the indices into smpl1 and smpl2 for both samples and for all four possibilites
# Christmas dates: 
# "2011-12-19", "2011-12-26", "2012-12-24", "2012-12-31", "2013-12-23", "2013-12-30", "2014-12-22", "2014-12-29", "2015-12-21", "2015-12-28"
# b) Do these curves belong to one of the outliers (for each possibility)? 
# c) Do a two sample mean equity test (for each possibility) for functions AND derivatives!

### TO DO for Philipp AND Christoph

################################################################
## Equity of Covariance for two samples
## Cf. Horvath/Kokoszka: Chapter 5
## split up data into two parts


################################################################
## Detection of changes in the mean function
## Cf. Horvath/Kokoszka: Chapter 6
## split up data into two parts


################################################################
## Portmanteau test of independence 
## Cf. Horvath/Kokoszka: Chapter 7
## split up data into two parts

## Products ##############################################################################
rla            			<- "SRL"
pricetype       		<- "LP" # "LP" or "AP"
PM 						<- c("POS", "NEG")
HTNT 					<- c("HT", "NT")

##########################################################################################
# Import/Adjust Data and define date properties ##########################################
########################################################################################## 
# Demand and Prices    
# Data Preparation

for(pm in PM){
  for(htnt in HTNT){
    print(paste0(pm, "_", htnt))
    assign(x = paste0("biddata_", pm, "_",htnt), 
    value = prepareData(quantilePath = paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,"_quantiles.csv"), 
                        bidPath = paste0("data/", rla, "_", pm, "_",htnt,"_", pricetype,".csv"), 
                        rla = rla, pm = pm, pricetype = pricetype, 
                        apLoss=380))
    
  }
} # now four objects for each combinations of pm and htnt, e.g.: data_l


summary(biddata_POS_HT$quantiles)
str(biddata_POS_HT, max = 1) 
# $ quantiles:'data.frame':      210 obs. of  10 variables:; 10 descriptives of prices like max, min, weighted averages, ...
# $ bids     :'data.frame':      127 obs. of  421 variables; # the data input
# $ dates    : Date[1:210], format: "2011-06-27" "2011-07-04" ... 210 curve observations
# $ mws      : int [1:127, 1:210] 10 20 25 50 75 90 100 110 125 135 ... for each bid the mw position
# $ price    : num [1:127, 1:210] 450 460 476 500 525 ... the single "LP" or "AP"
# $ bidSizes : num [1:127, 1:210] 10 10 5 25 25 15 10 10 15 10 ... # not clean bid sizes
# $ bidSizesc: num [1:128, 1:210] 0 10 10 5 25 25 15 10 10 15 ... # clean bid sizes
# $ nBids    : int [1:210] 60 57 68 71 74 41 73 46 68 72 ... # number of Bids in each week
# $ apLoss   : num 380 # apLoss for optimization
# $ price_hour: num [1:127, 1:210] 7.5 7.7 7.9 8.3 8.8 9.1 9.2 9.5 9.6 10 ...
# $ gavgh    : num [1:210] 14 14.3 14.1 13.8 13.2 ... # hourly weighted averages 
# $ maxh     : num [1:210] 17.3 15.6 15.1 14.4 14 ... # hourly maximum prices
# $ dates_mw : Date[1:20], format: "2011-06-27" "2011-10-03" ... # dates for MW demand changes
# $ demCap   :List of 5 # demanded Capacity plots for each capacity
# $ mwsc     : num [1:128, 1:210] 0 10 20 25 50 75 90 100 110 125 ...
# $ price_clean: num [1:128, 1:210] 450 450 460 476 500 ... # for stepcurves; starting with a double first observation
# $ holidays   :'data.frame':    210 obs. of  2 variables: # number of national german holidays from monday to friday 

##########################################################################################
# Global Parameters ######################################################################  
##########################################################################################
## Data Selection
type_selector		<- "POS_HT" # "NEG_HT", "POS_NT", "NEG_NT"
date_selector		<- "all" 	# or "wo_christmas", ..
#date_selector		<- "wo_christmas" 	# or "wo_christmas", ..

data_l       		<- switch(	type_selector,
								POS_HT   = biddata_POS_HT,
								POS_NT   = biddata_POS_NT,
								NEG_HT   = biddata_NEG_HT,
								NEG_NT   = biddata_NEG_NT)

christmas       	<- data_l$quantiles$christmas											

data_l       		<- switch(	date_selector,
								all   			= data_l,
								wo_christmas 	= list(	"dates" 		= data_l$dates[!(christmas == 1)],
														"nBids"			= data_l$nBids[!(christmas == 1)],
														"price"			= data_l$price[ , !(christmas == 1)],
														"quantiles"		= data_l$quantiles[!(christmas == 1) , ],
														"mws"			= data_l$mws[ , !(christmas == 1)],
														"mwsc"			= data_l$mwsc[ , !(christmas == 1)],
														"bids" 			= data_l$bids[, c(rep( !(christmas == 1), each = 2), FALSE)],
														"bidSizes" 		= data_l$bidSizes[ , !(christmas == 1)], 
														"bidSizesc" 	= data_l$bidSizesc[ , !(christmas == 1)], 
														"price_hour" 	= data_l$price_hour[ , !(christmas == 1)],
														"gavgh"			= data_l$gavgh[!(christmas == 1)],
														"maxh"			= data_l$maxh[!(christmas == 1)],
														"dates_mw" 		= data_l$dates_mw,
														"demCap"		= data_l$demCap,
														"price_clean"	= data_l$price_clean[ , !(christmas == 1)],
														"maxMW"			= data_l$maxMW[!(christmas == 1)],
														"holidays"		= data_l$holidays[!(christmas == 1)]))




														
str(data_l, max = 1)

## Number of Factors #####################################################################
K                 		<- 2  		# number of principal components; determines undersmoothing as well               
K_possible				<- 1:4		# sequence of possible factors of which we choose
n_ev_points    			<- 100 		# number of evaluation points for discretization of ymw_cov_hat_m and functions; 
									# ymw_cov_hat_m becomes a n_ev_points x n_ev_points matrix
mw_fixed      			<- 1000 	# fixed MW Position for fitted plots; can be checked with other quantiles
example_curves  		<- 100:105	# which curves should be plotted?

## Data Choice #########################################################################
dates           		<- as.Date(data_l$dates)
dates_int       		<- c(dates[1], dates[length(dates)])


n_bids          		<- data_l$nBids 				# number of bids per auction
N               		<- max(n_bids, na.rm = TRUE) 	# random number N_i
T 						<- length(dates)
lambda					<- 30							# Penalty Parameter for the case of monotone P Splines
start_cutting_point		<- 500							# MW Position which should be used as a cutting point for better covariance matrix
end_cutting_point		<- 1500




## Y_ij values
y_m           			<- data_l$price
y_orig_m          		<- data_l$price
y_orig_v          		<- as.vector(y_orig_m)
y_l 					<- Mat2List(y_orig_m, 2, na.rm = TRUE)            			
y_v 					<- unlist(y_l)

## X_ij values
# u_m           			<- data_l$mws
u_m           			<- data_l$mws_zero
u_orig_m          		<- u_m
u_orig_v          		<- as.vector(u_orig_m)
u_l						<- Mat2List(u_orig_m, 2, na.rm = TRUE)
u_v 					<- unlist(u_l)

u_order_v				<- order(u_v)
u_sort_v 				<- u_v[u_order_v]
y_sort_v				<- y_v[u_order_v]

##########################################################################################
## Define a training matrix for an index set, here outliers at christmas when demand is above 2300:
training_index 			<- which(sapply(u_l, max) > 2300)

## Common domain for all functions; max_domain including common domain for all functions
common_domain_int		<- apply( apply(u_m, 2, range, na.rm = TRUE), 1, min)           			
com_dom_discretization	<- seq( from = common_domain_int[1], to = common_domain_int[2], length.out = n_ev_points)
max_dom_discretization	<- unique(c( com_dom_discretization, seq(from = max(com_dom_discretization), to = range(u_v)[2], length.out=round((1-max(com_dom_discretization)/range(u_v)[2]) * n_ev_points,0))))														

cut_dom_discretization 	<- max_dom_discretization[max_dom_discretization > start_cutting_point & max_dom_discretization < end_cutting_point]


grid_list      			<- vector("list",2)		# list object for both dimensions
grid_list[[1]] 			<- seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points)
grid_list[[2]] 			<- seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points)

if( !all(dim(y_m) == dim(u_m)) ) stop("Dimensions do not fit!") # 

### Introduction
# Demanded Capacity:
#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "demanded_capacity.pdf"))
plot_dem_cap(data_l)
dev.off()


# Example Curves
#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "example_curves",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))
for(i in example_curves){
	plots[[i-min(example_curves)+1]] <- plot_step_curve(data_l, curve_index = i, smooth=F)
}
multiplot(plotlist = plots, cols=2)
dev.off()



#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "training_curves",min(training_index), "-", max(training_index) ,".pdf"), height = 20, width = 15)
par(mfrow=c(ceiling(length(training_index)/2),2))
plots 					<- vector("list", length = length(training_index))
counter 				<- 1
for(i in training_index){
	plots[[counter]] 		<- plot_step_curve(data_l, curve_index = i, smooth=F)
	counter 				<- counter +1
}
multiplot(plotlist = plots, cols=2)
dev.off()

##########################################################################################
## Bin Plots							     											##
##########################################################################################
## Bin Plot depending on lp Resolution (Intervalls = 2*lpResolution, mwLevelSeq for coloring; maximum 9 Intervalls)
## bidData of the structure from before with dates as bidData$dates and lps as bidData$lps
## color has to be something of colorbrewer
## in case of weeks
lpResolution 			<- 5
##  in case of hours:
## lpResolution <- 0.2
## types: type="bin", type="bidSize", type="mwPosition" 
mwLevelSeq 				<- c(seq(0,180,30),3000)
Xticks 					<- Get_xTicks(data_l$quantiles$dates)
# hightlight christmas demand change:
dates_christmas_df 		<- as.data.frame(cbind(	event = c("Christmas 2013", "Christmas 2014"), 
												startDate = c("2013-12-21", "2014-12-20"), #two days before first acution
												endDate = c("2014-01-01", "2014-12-31")), # four days after second auction
												rownames = NULL, stringsAsFactors = FALSE)
dates_christmas_df$startDate <- as.Date(dates_christmas_df$startDate)
dates_christmas_df$endDate 	<- as.Date(dates_christmas_df$endDate)

lpResolution 			<- 5
mwLevelSeq   			<- c(seq(0,180,30),3000)
max_price    			<- c(	"POS_HT" = FALSE, 
								"POS_NT" = 2500,
								"NEG_HT" = 3000,
								"NEG_NT" = 3200)

for(pm in PM){

  for(htnt in HTNT){
    x         				<- get(paste0("biddata_", pm, "_",htnt)) # bidData
    
    Xticks    				<- Get_xTicks(x$quantiles$dates)
    filename  				<- paste0(rla, "_", pm, "_",htnt)
    cust      				<- customizeProduct(filename) # customize colours

    assign( 	x = 	paste0("binplot_", pm, "_",htnt), 
				value = getPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
								filename = filename, pricetype, max_price = max_price[paste0(pm, "_",htnt)],
                        rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F) ) 
						
	# assign( 	x = 	paste0("binplot_", pm, "_",htnt), 
				# value = getPlot(lpResolution, mwLevelSeq, bidData = x, color=cust$color[1], type="bin", 
								# filename = filename, pricetype, max_price = max_price[paste0(pm, "_",htnt)],
                        # rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F) +
						# geom_rect(	data = dates_christmas_df, # add grey shades
									# aes(NULL, NULL, xmin = startDate, xmax = endDate, colour = NULL), # unmap colour
									# ymin = -Inf, ymax = +Inf, fill = "gray80", alpha = 0.8))  
  }
} # now four objects for each combinations of pm and htnt, e.g.:

pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "bin_plot_all.pdf"), width = 20)
grid.arrange(	binplot_POS_HT, binplot_POS_NT, 
				binplot_NEG_HT, binplot_NEG_NT, nrow=2)
dev.off()
  
	   
##########################################################################################	   
# 3d Plot of all
plot_3d_contour(data_l, 
				filename=paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "contour.pdf"), 
				snapshot=TRUE)
dev.off()

##########################################################################################
##########################################################################################
## 1. STEP: Estimation of price functions     											##
## Pre-Smoothing of the functions        												##
## under-smoothed:    X_usmth_l          												##
## GCV-smoothed:      X_gcvsmth_l /median of all smoothing parameters spar             	##
##########################################################################################
##########################################################################################

X_gcvsmth_l        		<- vector(mode = "list", length = T)

##########################################################################################
## Determination of GCV-smoothing parameter:
gcv_values 				<- rep(NA, times = T)

for(t in 1:T){
  print( paste0("At t = ", t ," the length of u = ", length(u_m[,t][ !is.na(u_m[,t]) ]), "; y = ", length(y_m[,t][ !is.na(y_m[,t]) ])))
  gcv_values[t] 			<- 	sm.spline(	x = u_m[,t][ !is.na(u_m[,t]) ], 
											y = y_m[,t][ !is.na(y_m[,t]) ])$spar
}
summary(gcv_values)
#spar_gcv 				<- median(gcv_values)
# spar_gcv 				<- mean(gcv_values)
spar_gcv 				<- 100000


print(paste0("GCV Median of ",T," price curves: ", round(spar_gcv , -floor(log10(spar_gcv))) , " (rounded)")) 


##########################################################################################
## GCV-Smoothing: median of all gcv values; here: 6255.381
for(t in 1:T){ # t <- 183
  X_gcvsmth_l[[t]] 			<- sm.spline(	x = u_m[,t][ !is.na( u_m[,t]) ],
											y = y_m[,t][ !is.na( y_m[,t]) ], 
											spar = spar_gcv) # s par = smoothing parameter
}


														

# GCV Smoothing of Example Curves
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0),"_curves",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))

for(i in example_curves){
  print(paste0(i-min(example_curves)+1, ": ", dates[i]))
  u_prediction 				<- seq( from = min(u_m[ ,i], na.rm=T), to = max(u_m[ ,i], na.rm=T), length.out = n_ev_points)
  y_smoothed  				<- predict(X_gcvsmth_l[[i]], u_prediction)
  smooth_data 				<- data.frame(y = y_smoothed, x = u_prediction)
  
  plots[[i-min(example_curves)+1]] <- plot_step_curve(bid_data = data_l, curve_index = i, smooth=TRUE, smooth_data=smooth_data)
}
multiplot(plotlist = plots, cols=2)
dev.off()

## Monotone PSpline Smoothing ############################################################
observed_short_domain_l		<- vector("list",T) # a list containing the same x_ij points which are equidistant but of course different from curve to curve

X_monpsp_ci_l				<- vector("list",T) # common intervall with monotone p splines smoothed functions
X_monpsp_prime_ci_l			<- vector("list",T) # same but smoothed derivatives
X_monpsp_sp_l				<- vector("list",T) # the same on sparse u_l[[t]]
X_monpsp_prime_sp_l			<- vector("list",T)
			
for( t in 1:T){ # example_curves t <- 100
	print(t)
	psline_sp 						<- monotPSpline( y=  y_l[[ t ]], x = u_l[[ t ]], lambda = lambda) # evaluated at u_l[[i]]
	X_monpsp_prime_sp_l[[t]]		<- psline_sp$predict.firstDeriv.c
	X_monpsp_sp_l[[t]]				<- psline_sp$eval.basis %*% psline_sp$solution$constrained
	
	observed_short_domain  			<- max_dom_discretization[max_dom_discretization <= max(u_m[,t][ !is.na( u_m[,t]) ])]
	observed_short_domain_l[[t]]	<- observed_short_domain
	psline_ci 						<- monotPSpline( y=  y_l[[ t ]], x = u_l[[ t ]], eval_x = max_dom_discretization, lambda = lambda) # evaluated at equidistant short domain
	
	X_monpsp_prime_ci_l[[t]]		<- psline_ci$predict.firstDeriv.c
	X_monpsp_ci_l[[t]]				<- psline_ci$eval.basis %*% psline_ci$solution$constrained
}	

# Monotone P Splines of Example Curves
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "pspline_example_wlambda_", lambda, "_",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
par(mfrow=c(ceiling(length(example_curves)/2),2)) # example_curves <- 100:105
plots 					<- vector("list", length = length(example_curves))
for(t in example_curves){ # t <- 100
  smooth_data 				<- data.frame(y = X_monpsp_sp_l[[t]], x = u_l[[ t ]])
  plots[[t-min(example_curves)+1]] <- plot_step_curve(bid_data = data_l, curve_index = t, smooth=TRUE, smooth_data=smooth_data)
}
multiplot(plotlist = plots, cols=2)
dev.off()

## 
X_monpsp_prime_sp_fit	<- locfit(unlist(X_monpsp_prime_sp_l)[u_order_v]  ~  lp( u_sort_v  , nn = 0.7, h = 0 ) )
X_monpsp_prime_sp_pred 	<- predict(X_monpsp_prime_sp_fit, newdata = grid_list[[1]])

X_monpsp_prime_ci_fit 	<- locfit(unlist(X_monpsp_prime_ci_l)  ~  lp( rep(max_dom_discretization, times = T)  , nn = 0.7, h = 0 ) )
X_monpsp_prime_ci_pred 	<- predict(X_monpsp_prime_sp_fit, newdata = grid_list[[1]])

## Undersmoothing values #################################################################
usmth_K1   	  			<- 1 * 10^(floor(log10(spar_gcv))) # one power below the median times 5
usmth_K2     			<- 3 * 10^(floor(log10(spar_gcv))) # one power below the median times 7
usmth_K3      			<- 5 * 10^(floor(log10(spar_gcv)))     # same power but times 1

spar_usmth 				<- switch(as.character(K),
									"1" = usmth_K1 ,
									"2" = usmth_K2 ,
									"3" = usmth_K3 )

X_usmth_l       		<- vector("list",T)

for(t in 1:T){
  ## undersmoothing using the cross validation undersmoothing parameter
  X_usmth_l[[t]]   			<- sm.spline(	x = u_m[,t][ !is.na( u_m[,t]) ],
											y = y_m[,t][ !is.na( y_m[,t]) ], 
											spar = spar_usmth)
}

# Under Smoothing of Example Curves
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "usmth_example_curves",min(example_curves), "-", max(example_curves) ,".pdf"), height = 20, width = 15)
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))
for(i in example_curves){
  print(paste0(i-min(example_curves)+1, ": ", dates[i]))
  u_prediction 				<- seq( from = min(u_m[ ,i], na.rm=T), to = max(u_m[ ,i], na.rm=T), length.out = n_ev_points)
  y_smoothed  				<- predict(X_usmth_l[[i]], u_prediction)
  smooth_data 				<- data.frame(y = y_smoothed, x = u_prediction)
  plots[[i-min(example_curves)+1]] <- plot_step_curve(bid_data = data_l, curve_index = i, smooth=TRUE, smooth_data=smooth_data)
}
multiplot(plotlist = plots, cols=2)
dev.off()

X_gcv_prime_sp_l 		<- vector("list", length = T)
X_usmth_prime_sp_l 		<- vector("list", length = T)
X_gcv_sp_l 				<- vector("list", length = T)
X_under_sp_l 			<- vector("list", length = T)


# SP for sparse case
for( t in 1:T){
		X_gcv_prime_sp_l[[t]] 		<- predict(X_gcvsmth_l[[t]],  	xarg = u_m[,t][ !is.na( u_m[,t]) ],  nderiv = 1)
		X_usmth_prime_sp_l[[t]]		<- predict(X_usmth_l[[t]],  	xarg = u_m[,t][ !is.na( u_m[,t]) ],  nderiv = 1)
		X_gcv_sp_l[[t]]				<- predict(X_gcvsmth_l[[t]],  	xarg = u_m[,t][ !is.na( u_m[,t]) ],  nderiv = 0)
		X_under_sp_l[[t]]			<- predict(X_usmth_l[[t]],  	xarg = u_m[,t][ !is.na( u_m[,t]) ],  nderiv = 0)
}
##########################################################################################
## Plots of fitted price curves: Univariate time series of fitted price-demand functions evaluated at u = u_fix MW 
##########################################################################################

price_usmth				<- NULL
price_gcvsmth 			<- NULL

for(t in 1:T){
    price_usmth 			<- c(price_usmth, predict(X_usmth_l[[t]], mw_fixed))
    price_gcvsmth 			<- c(price_gcvsmth, predict(X_gcvsmth_l[[t]], mw_fixed))
}

plot(price_usmth, type = "l")
dev.off()
plot(price_gcvsmth, type="l")
dev.off()

##########################################################################################
## Discretization of price-demand functions at the observed values of electr. demand #####
##########################################################################################

x_tilde_usmth_m			<- matrix(NA, N, T) # undersmoothed
x_hat_gcvsmth_m			<- matrix(NA, N, T) # gcv smoothed
tmp          	 		<- matrix(NA, N, T) # equidistant grid of length N

for(t in 1:T){  # equidistant discretization
	tmp[,t]          	  	<- seq( from = min(u_m[,t],na.rm=T), to = max(u_m[,t],na.rm=T), length.out = N)
	x_tilde_usmth_m[,t] 	<- predict(X_usmth_l[[t]],  tmp[,t] )
	x_hat_gcvsmth_m[,t]		<- predict(X_gcvsmth_l[[t]], tmp[,t] )  
}

u_m          			<- tmp
x_tilde_usmth_v 		<- as.vector(x_tilde_usmth_m)
x_hat_gcvsmth_v 		<- as.vector(x_hat_gcvsmth_m)


##########################################################################################
## Standardization of the discretized price-demand functions #############################
##########################################################################################
## ToDo: Mittelwert abziehen
## Scaling-parameter: c_t for ||\hat{X}_t||
c_t       				<- numeric(T)
for(t in 1:T){
	x_hat_gcvsmth_t 		<- x_hat_gcvsmth_m[,t][!is.na(x_hat_gcvsmth_m[,t])]
	x_hat_gcvsmth_t_sq  	<- (x_hat_gcvsmth_t^2)
	c_t[t]     				<- sqrt(sum(x_hat_gcvsmth_t_sq))
}

# x_hat_scaled_m becomes scaled, i.e.: x_hat_gcvsmth_t/c_t
x_hat_scaled_m    		<- matrix(nrow = N, ncol = T)
x_tilde_scaled_m 		<- matrix(nrow = N, ncol = T)

## standardization; both the tildes and the hats with the same c_t:
for(t in 1:T){
  x_hat_scaled_m[,t][!is.na(x_hat_gcvsmth_m[,t])]   	<- c(x_hat_gcvsmth_m[,t][!is.na(x_hat_gcvsmth_m[,t])])/c_t[t]
  x_tilde_scaled_m[,t][!is.na(x_tilde_usmth_m[,t])]   	<- c(x_tilde_usmth_m[,t][!is.na(x_tilde_usmth_m[,t])])/c_t[t]
}

## Summary:
# x_tilde_usmth_m is the originally undersmoothed matrices (with vector version x_tilde_usmth_v) while
# x_tilde_scaled_m is by the c_t undersmoothed scaled matrix  (with vector version x_tilde_scaled_v) 

x_tilde_scaled_v  		<- as.vector(x_tilde_scaled_m) 	# X_t^star tilde
x_hat_scaled_v    		<- as.vector(x_hat_scaled_m) # X_t^star hat



##########################################################################################
##########################################################################################
## 2. STEP: Computation of Covariance Function Gamma (see Yao MÃ¼ller & Wang JASA 2005)  ##   											
##########################################################################################
##########################################################################################

## Preparation of data:
## \tilde{X}_t^\ast(u_{ti}) *  \tilde{X}_t^\ast  (u_{tj})
## for local linear surface smoothing (see equation 10, page 23)


ymw_covariance_ar   	<- array(NA, dim=c(N, N, T)) 	# N x N x T Array for Covariance Gamma hat; Yao Müller Wang matrix
diagonal_m      		<- matrix(NA,N,N)				    # N x N diagonal matrix of Gamma
diag(diagonal_m) 		<- rep(1,N)						# N diagonal entries of Gamma		
diagonal_rm_m     		<- matrix(1,N,N)+diag(NA, N, N) # N x N matrix of ones with NAs on diagonal

for(i in 1:T){
  ymw_covariance_ar[ , , i] <- (x_tilde_scaled_m[,i] %*% t(x_tilde_scaled_m[,i])) * diagonal_rm_m
}

u_1_m        			<- u_m[rep(1:N, N),     ] # stack 1:N N times
u_2_m        			<- u_m[rep(1:N, each=N),] # repeat each enty of 1:N N times
u_ordered_ar    	 	<- array(dim=c(N,N,T))
u_1_ar 					<- array(dim=c(N,N,T))
u_2_ar 					<- array(dim=c(N,N,T))


for(i in 1:T){
  ## N*N-vec 2 NxN-mat
  u_ordered_ar[,,i] 		<- u_1_m[,i]
  u_1_ar[,,i] 				<- u_1_m[,i]
  u_2_ar[,,i] 				<- u_2_m[,i]
}

## Set outer cols/rows (and diagonal) to NA
for(i in 1:T){
  u_1_ar[,,i] 				<- u_1_ar[,,i] * diagonal_rm_m # non normal matrix multiplication
  u_2_ar[,,i] 				<- u_2_ar[,,i] * diagonal_rm_m
  u_ordered_ar[,,i]    		<- u_ordered_ar[,,i]    * diagonal_m
}

u_1_v    				<- as.vector(u_1_ar)
u_2_v      				<- as.vector(u_2_ar)
ymw_covariance_v       	<- as.vector(ymw_covariance_ar)


##########################################################################################
## Computation of emp. covariance-function gamma by local surface smoothing ##############
##########################################################################################
 # G.vec holds the values: \hat{X}_t^\ast (u_{ti})* \hat{X}_t^\ast (u_{tj})

ymw_cov_hat_loc          <- locfit(ymw_covariance_v  ~  lp( u_1_v, u_2_v , nn = 0.7, h = 0 ) )
summary(ymw_cov_hat_loc)

## Plot Covariance-Function gamma-hat
## Evaluation grid for the plot of the covariance function:

## Evaluation
ymw_cov_hat_m     		<- matrix(predict(ymw_cov_hat_loc , newdata=grid_list), nrow = n_ev_points, ncol = n_ev_points)

##########################################################################################
## Plots of Covariance matrix
##########################################################################################
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "covariance.pdf"), height = 20, width = 15)
hgt 					<- 0.25 * (ymw_cov_hat_m[-n_ev_points,-n_ev_points] + ymw_cov_hat_m[-1,-n_ev_points] + ymw_cov_hat_m[-n_ev_points,-1] + ymw_cov_hat_m[-1,-1])
hgt 					<- (hgt - min(hgt))/ (max(hgt) - min(hgt))

scl     				<- 1
scl.axs 				<- 1.4
scl.lab 				<- 1.4
par(oma = c(0,0,0,0), mar = c(0,1.8,0,1.1), cex = scl, cex.lab = scl.lab, cex.axis = scl.axs)
persp.m 				<- persp(	x = grid_list[[1]], y = grid_list[[2]], z = ymw_cov_hat_m,
									main = "", xlab = "MW", ylab = "MW",zlab = "", 
									col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
									ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 3)
dev.off()

##########################################################################################
## FPCA ##################################################################################
##########################################################################################

demand_length 			<- max(u_m, na.rm=TRUE)-min(c(u_m, 0), na.rm=TRUE) 	# demanded MW intervall length 
h            			<- demand_length / (n_ev_points - 1) 			# step length
w            			<- rep(h, times = n_ev_points)
W            			<- diag(w) 											# diagonal n_ev_points matrix with diagonal element w 

# singular value decomposition of sqrt(W) * ymw_cov_hat_m * sqrt(W)
eigen_decomp      		<- eigen( sqrt(W) %*% ymw_cov_hat_m %*% sqrt(W) , symmetric=TRUE) 
eigen_values        	<- eigen_decomp$values									# eigenvalues of sqrt(W) * ymw_cov_hat_m * sqrt(W)
eigen_vector	       	<- eigen_decomp$vectors									# eigen vectors of sqrt(W) * ymw_cov_hat_m * sqrt(W)

sum(eigen_values) 																			# complete variance explained (with numerical problems!)
round(sum(eigen_values[1:2])/sum(eigen_values)*100 ,2)												# 102% -> ocurring problem with negative values
round(sum(eigen_values[eigen_values > 1][1])/sum(eigen_values[eigen_values > 0])*100 ,2)								# 99.94% by restriction to positive eigenvalues

## Shares on total Variance
(var_shares   			<- (eigen_values[eigen_values>0])/sum(eigen_values[eigen_values>0]))
round(var_shares[1:4],digits=4)*100
cumsum(round(var_shares[1:10],digits=4)*100)

n_positive_ev 			<- length(eigen_values[eigen_values > 0])								# 6 of n_ev_points eigenvalues are positive

## Create ScreePlot
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "Screeplot.pdf"))
createScreePlot(cumsum(round(var_shares[1:10],digits=4)*100))
dev.off()

##########################################################################################
## Interpolation of Eigenvectors "eigen_vector_ast":
eigen_vector_ast    	<- solve(sqrt(W)) %*% eigen_vector
## Check Size:
t(eigen_vector_ast[,1]) %*% W %*% eigen_vector_ast[,1]

eigen_function_hat      <- smooth.Pspline(	x = seq( from = min(u_m, na.rm=TRUE), to = max(u_m, na.rm=TRUE), length = n_ev_points),
											y = eigen_vector_ast[, 1:n_ev_points ]) # only positive ev smoothing
# summary(eigen_function_hat)


											
# str(eigen_function_hat)
# eigen_function_hat$ysmth[,6] 	# first smoothed and evaluated eigenfunction
# eigen_function_hat$ysmth

##########################################################################################										 
## Eigenfunctions
n_eigenfunctions 		<- 6
n_eigenfunctions		<- min(n_positive_ev, n_eigenfunctions)

# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "presmoothed_eigenfunctions.pdf"), height = 20, width = 15)
par(mfrow=c(n_eigenfunctions/2,2))
for(ef in 1:n_eigenfunctions){
	plot(	x = seq(0, 1, length=n_ev_points),
			y = rep(0, length=n_ev_points),
			col = "red", type = "l", main = paste0("Eigenfunction ", ef ,"\n", round(var_shares[ ef ]*100, digits=4),"%"),
			ylab = "EUR/MWh", xlab = "Adjusted Demand (MW)",
			ylim = range(	c(3*predict(eigen_function_hat, seq(min(u_m, na.rm=TRUE),max(u_m, na.rm=TRUE),length = n_ev_points))[, ef ],
								-3*predict(eigen_function_hat, seq(min(u_m, na.rm=TRUE),max(u_m, na.rm=TRUE),length = n_ev_points))[, ef ])))
		lines(	x = seq(0, 1, length = n_ev_points),
				y = (3*predict(eigen_function_hat, seq(min(u_m, na.rm=TRUE),max(u_m, na.rm=TRUE),length = n_ev_points))[, ef ]))
		lines(	x = seq(0, 1, length = n_ev_points),
				y = (-3*predict(eigen_function_hat, seq(min(u_m, na.rm=TRUE),max(u_m, na.rm=TRUE),length = n_ev_points))[, ef ]))
	
	}
dev.off()

##########################################################################################	
## Evaluate eigen-functions at observed grids of electricity-demand (u_{th})

phi_i_hat            	<- vector("list", T)
for(t in 1:T){
  tmp             		<- u_m[,t][!is.na(u_m[,t])]
  phi_i_hat[[t]]     	<- predict(eigen_function_hat, tmp)
}


## Plot the n_eigenfunctions phi_i_hat for example curves
#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "phi_hat for example curves.pdf"))
cols 					<- rainbow(n_eigenfunctions)
i <- min(example_curves)
plot( 	x = na.omit(u_m[ ,i]), y = phi_i_hat[[i]][ , 1], col = cols[1], type = "l",  
		xlim = range(na.omit(u_m[ ,example_curves])), ylim = range( sapply(phi_i_hat, function(x) range(x[, 1:n_eigenfunctions]))[ , example_curves]),
		ylab = paste0("Phi_hat for week ", dates[i]), xlab = "Demand (MW)" ) + 
for(j in 2:n_eigenfunctions){
	print(paste0("j = ", j , " and i= ", i))
	lines( x = na.omit(u_m[ ,i]), y = phi_i_hat[[i]][ , j], col = cols[j]) 
	}
legend("topright", col = cols , lty=1 , legend=paste0(1:6, ". phi_hat"))

dev.off()


##########################################################################################
## Determination of Factors K ############################################################
## AIC of Yao et al. 2005 ################################################################
##########################################################################################

##########################################################################################
## Estimation: Standard deviation of additive error term 
tmp 					<- NULL
for(t in 1:T){
	tmp 					<- c(tmp, as.vector( predict(X_gcvsmth_l[[t]], u_orig_m[,t][!is.na(u_orig_m[,t])]) ))
}

y_v          	 		<- c(y_m)


if(length(tmp) != length(y_v[!is.na(y_v)]) || length(tmp) != length(u_orig_m[!is.na(u_orig_m)])) stop("Lengths of vectors does not correspond!")

tmp 					<- ( y_v[!is.na(y_v)] - tmp )^2

sig2_hat 				<- mean(tmp[ tmp < quantile(tmp, p = 0.95) ])
	
y_aic_v     			<- as.vector(na.omit(y_orig_m))
u_aic_v     			<- as.vector(na.omit(u_orig_m))

y_aic_m      			<- u_orig_m
u_aic_m      			<- y_orig_m

##########################################################################################
## Determination for different values k for K components in K_possible

aic_for_possible_K  	<- vector("list", length(K_possible))

for(k in K_possible){
	# each row a new factor K k =2 t 
	scores     				<- matrix(NA, nrow = k, ncol = T)
	for(t in 1:T){
		y 						<- as.vector(x_tilde_scaled_m[,t][!is.na(x_tilde_scaled_m[,t])])
		# regression formula: y ~ 0 +  as.vector(phi_i_hat[[t]][, 1]) + ... + as.vector(phi_i_hat[[t]][, k])
		regression_formula 		<- as.formula(paste0("y ~ 0  ", paste0("+ as.vector(phi_i_hat[[t]][,", seq(from = 1, to = k) ,"])", collapse = "")	))
		tmp.lm     				<- summary(lm(	regression_formula )) # tmp.lm
		scores[,t] 				<- tmp.lm$coefficients[,1]
	}


	residual_m         		<- matrix(NA, nrow = dim(u_aic_m)[1], ncol = dim(y_aic_m)[2])

	for(t in 1:T){
		all_k_prediction 		<- 0
		# in all_k_preidiction there is the sum of all predictions times scores times c_t from 1 to k, therefore k summands
		for(i in 1:k){
			kth_prediction 			<- predict( eigen_function_hat , u_aic_m[,t][ complete.cases(u_aic_m[,t]) ] )[, i ]
			all_k_prediction 		<- all_k_prediction + kth_prediction * scores[ i , t ] * c_t[t]
		}
		residual_m[,t][!is.na(u_aic_m[,t])] 	<- 	c(y_aic_m[,t][complete.cases(u_aic_m[,t])]) - all_k_prediction	
	}

	AIC 					<- apply(X = residual_m, MARGIN = 2, FUN = sum, na.rm = T)

	N_t.log.2pi_2    		<- rep(N,T)*log(2*pi)/2
	N_t.log.sig2_hat_2		<- rep(N,T)*log(sig2_hat)/2
	
	AIC_sig2_hat 			<- AIC/(2 * sig2_hat)

	L 						<- sum(c(-N_t.log.2pi_2 - N_t.log.sig2_hat_2 - AIC_sig2_hat))
	
	aic_for_possible_K[[k]] <- list( scores = scores, residual_m = residual_m, AIC = AIC, AIC_sig2_hat = AIC_sig2_hat, L = L , AIC_yao = -L + k) 
}

aic_yao 				<- sapply(aic_for_possible_K, function(x) x$AIC_yao)

# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "aic_yao.pdf"))
plot(aic_yao, type="l", main = "AIC by Yao")
dev.off()

## As differences
round(aic_yao[1] - aic_yao[2], digits=1)
round(aic_yao[1] - aic_yao[3], digits=1)
round(aic_yao[1] - aic_yao[4], digits=1)
round(aic_yao[2] - aic_yao[3], digits=1)
round(aic_yao[2] - aic_yao[4], digits=1)
round(aic_yao[3] - aic_yao[4], digits=1)


## Check Variance-Shares of first K eigenvalues
cumsum(round(var_shares[1:3],digits=4)*100)

##########################################################################################
## choose k_opt to minimize Yaos AIC
k_opt <- which.min(aic_yao)
if(k_opt != K) print(paste0("AIC Optimal K =", k_opt, " does not equal the in the beginning chosen K = ", K)) 


##########################################################################################
## Determine projection parameters \beta_{tk} by Least-Squares (here: \beta_{th} are called "scores"
##########################################################################################

scores     				<- matrix(NA, nrow = k_opt , ncol = T)
for(t in 1:T){
	y 						<- as.vector( x_hat_gcvsmth_m[,t][!is.na(x_hat_gcvsmth_m[,t])] )
	# regression formula: y ~ 0 +  as.vector(phi_i_hat[[t]][, 1]) + ... + as.vector(phi_i_hat[[t]][, k])
	regression_formula 		<- as.formula(paste0("y ~ 0  ", paste0("+ as.vector(phi_i_hat[[t]][,", seq(from = 1, to = k_opt) ,"])", collapse = "")	))
	
	tmp.lm  				<- summary(lm(regression_formula))
	scores[,t] 				<- tmp.lm$coefficients[,1]
}

##########################################################################################
## Plot time serie of scores: \beta_{t1} and \beta_{t2}
## These are the non-rotated versions!
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "non_rotated_scores.pdf"))
par(mfrow=c(2,1))
plot.ts(scores[1,])
plot.ts(scores[2,])
dev.off()

save(scores, file = "scores.RData")
##########################################################################################
## Residuals 

y_res_v       			<- as.vector(y_orig_m)
u_res_v       			<- as.vector(u_orig_m)

#u_res_v[ !outler_index ] <- NA
#y_res_v[ !outler_index ] <- NA

y_res_m       			<- y_orig_m
u_res_m       			<- u_orig_m

residual_m 				<- matrix(NA, dim(y_orig_m)[1], dim(y_orig_m)[2])

for(t in 1:T){
		all_k_prediction	<- 0
		# in all_k_preidiction there is the sum of all predictions times scores times c_t from 1 to k, therefore k summands
		for(i in 1:k_opt){
			kth_prediction 		<- predict( eigen_function_hat , u_res_m[,t][ complete.cases(u_res_m[,t]) ] )[, i ]
			all_k_prediction 	<- all_k_prediction + kth_prediction * scores[ i , t ]
		}
		residual_m[,t][!is.na(u_res_m[,t])] <- 	c( y_res_m[,t][complete.cases(u_res_m[,t])] ) - all_k_prediction	
	}

res_v  			 		<- as.vector(residual_m)
res_v   				<- res_v[!is.na(res_v)]
y_res_v 				<- y_res_v[!is.na(y_res_v)]

##########################################################################################
## R-square (R^2)   

SS.total  				<- sum(c(y_res_v - mean(y_res_v))^2)
R.sqr     				<- 1 - sum(c(res_v)^2)/SS.total

round(R.sqr, digits=2)


##########################################################################################
## VARIMAX Rotation of the Basisfunktions ################################################
##########################################################################################

##########################################################################################
## Discretize Eigenfunctions
## Values of first K functions at values of u_m
B              	 		<- predict(eigen_function_hat, seq( from = min(u_m, na.rm=TRUE), to = max(u_m, na.rm=TRUE), length.out = n_ev_points))[,1:K]

## VARIMAX-Rotation
A               		<- varimax(B)$loadings

## Interpolation of rotated eigenfunctions
rot_eigen_function_hat  <- smooth.Pspline( 	x = seq( from = min(u_m, na.rm=TRUE), to = max(u_m, na.rm=TRUE), length.out = n_ev_points),
											y = A)

## Compuation of new eigenvalues
rot_eigen_values	 	<- numeric(K)

for(k in 1:K){
	rot_disc_eigen_function_hat <- matrix(	predict(rot_eigen_function_hat,
											seq( from = min(u_m, na.rm=TRUE), to = max(u_m, na.rm=TRUE), length.out = n_ev_points))[,k], ncol=1)
	rot_eigen_values[k] 		<- t(rot_disc_eigen_function_hat) %*% W %*% ymw_cov_hat_m %*% W %*% rot_disc_eigen_function_hat
}

## explained variance-shares
rot_var_shares 			<- rot_eigen_values/(sum(eigen_values[ eigen_values>0 ]))

## comparision of orig. var-shares and rotated var-shares
rot_eigen_values/(sum(eigen_values[eigen_values>0])) # rotated
sum(rot_eigen_values/(sum(eigen_values[eigen_values>0])))

eigen_values[1:K]/(sum(eigen_values[eigen_values>0])) # original
sum(eigen_values[1:K]/(sum(eigen_values[eigen_values>0])))

## Rotated Var-shares
round(rot_var_shares,digits=4)*100
round(sum(rot_var_shares) ,digits=4)*100
round(sum(var_shares[1:k_opt]) ,digits=4)*100

## rotation of scores 
rot_mat      			<- varimax(B)$rotmat
rot_scores 				<- t(t(scores[1:K,])%*%rot_mat)

#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "rotated_scores.pdf"))
par(mfrow=c(2,1))
plot.ts(rot_scores[1,])
plot.ts(rot_scores[2,])
dev.off()

## Difference to non-rotated scores:
#pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "both_scores.pdf"))
par(mfrow=c(2,1))
plot.ts(cbind(rot_scores[1,], scores[1,]), col=c("black", "blue"), plot.type = "single")
plot.ts(cbind(rot_scores[2,], scores[2,]), col=c("black", "blue"), plot.type = "single")
dev.off()

##########################################################################################
## Plot of the rotated eigenfunctions; Fig 3 (right panel): 							##
##########################################################################################
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "rotated_eigenfunctions.pdf"))
scl     				<- 1
scl.axs 				<- 1.4
scl.lab 				<- 1.4

scale1 					<- mean(rot_scores[1,])
scale2 					<- mean(rot_scores[2,])
par(mfrow=c(1,1))
plot(	x = seq( from = min(u_m, na.rm=TRUE), to = max(u_m, na.rm=TRUE), length = n_ev_points),
		y = rep(0, length = n_ev_points),
		cex = scl, cex.lab = scl.lab, cex.axis = scl.axs, col = "white", type = "l", ylab = "EUR/MWh", xlab = "MW",
		ylim = 	c(-40,range(c(
					scale1*predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points))[,1],
					scale1*predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points))[,1],
					scale2*predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points))[,2],
					scale2*predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points))[,2])
					)[2])
     )
	lines( 	x = seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length=n_ev_points),
			y = scale1 * predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length = n_ev_points))[,1], lty = 1)
	lines( 	x = seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length = n_ev_points),
			y = scale2 * predict(rot_eigen_function_hat, seq(min(u_m, na.rm=TRUE), max(u_m, na.rm=TRUE), length = n_ev_points))[,2], lty = 2)
dev.off()

##########################################################################################
## Plot of scores																		##
##########################################################################################
## The rotated eigenfunctions are determined up do sign-changes.
## Here: Convenient sign-choice such that both time series of
## Scores have positive trends
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "rotated_scores_1plot.pdf"))
scl     				<- 1
scl.axs 				<- 1
scl.lab 				<- 1

sgn1 					<- ifelse(rot_scores[1,1] < rot_scores[1,T], 1 , -1 )
sgn2 					<- ifelse(rot_scores[2,1] < rot_scores[2,T], 1 , -1 )

# score * sign
rot_sign_score1 		<- rot_scores[1,]*sgn1 
rot_sign_score2 		<- rot_scores[2,]*sgn2


## Plot:
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "scores_together.pdf"))
par(mar=c(5.1, 5.1, 4.1, 5.1), mfrow=c(1,1), cex.lab=1.2, cex.axis=1.2, cex=1.2)

plot.ts(rot_sign_score1, lwd = 1.125, main = "", ylab = "",
        xlab = "Weekly Auctions (June, 2011 - June, 2015)", ylim = c(-45000, 0), axes = FALSE, frame = TRUE)
abline(v = 521, col = "red", lwd = 2)
axis(2, at = seq(2000, 28000,by=2000))
axis(1, cex.lab = 1.0)
axis(3, at = (521), label="January 1, 2008", col.lab="red", padj = 1, cex.lab=scl.lab, col.ticks="red", lwd.ticks=2, tick=T)

par(new=TRUE)
plot.ts(rot_sign_score2,main="",ylab="",
        xlab="",ylim = c(-45000, 0),axes=FALSE,frame=TRUE,lty=2,lwd=1.125)
axis(4,at=seq(2000,11000,by=2000))

legend("topleft",col=c("black","black"),lty=c(1,2),legend=c("1. Scores","2. Scores"),lwd=1.125)
dev.off()

##########################################################################################
##########################################################################################
## 3. STEP: Predict 1. Derivative on equidistant 										##
## a) basis on common intervall
##########################################################################################
##########################################################################################
## Reset u_m
u_m						<- u_orig_m

## Use GCV smoothed functions for this (gcv smoothing par = median of all gcvs = 6255)
#X_gcvsmth_l  
## Alternative: Use Undersmoothed functions for this (smoothing par = 5000)
#X_usmth_l

X_gcv_prime_ci_l 		<- vector("list", length = T)
X_usmth_prime_ci_l 		<- vector("list", length = T)
X_gcv_ci_l 				<- vector("list", length = T)
X_under_ci_l 			<- vector("list", length = T)


#CI for common intervall
for( t in 1:T){
		X_gcv_prime_ci_l[[t]] 	<- predict(X_gcvsmth_l[[t]],  	xarg = com_dom_discretization,  nderiv = 1)
		X_usmth_prime_ci_l[[t]]	<- predict(X_usmth_l[[t]],  	xarg = com_dom_discretization,  nderiv = 1)
		X_gcv_ci_l[[t]]			<- predict(X_gcvsmth_l[[t]],  	xarg = com_dom_discretization,  nderiv = 0)
		X_under_ci_l[[t]]		<- predict(X_usmth_l[[t]],  	xarg = com_dom_discretization,  nderiv = 0)
}



##########################################################################################
## Plots of pooled functions and 1st derivatives on common and discrete intervall		##
########################################################################################## 
## Pooled predicted functions on common intervall
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0),"usmth_presm_0stder_pooled.pdf")) 
plot(x = com_dom_discretization, y = X_gcv_ci_l[[1]], type = "l", pch=16, cex = 0.5, ylim = c(0, 1300), ylab = "Presmoothed Function")
abline( v = common_domain_int, col = "red")
for(t in 2:T){
	lines(x = com_dom_discretization, y = X_gcv_ci_l[[t]], pch=16, cex = 0.5)
}
for(t in 1:T){
	lines(x = com_dom_discretization, y = X_under_ci_l[[t]], lty="dotted", cex = 0.3, col = "red")
}
for(t in 1:T){
	lines(x = observed_short_domain_l[[t]], y = X_monpsp_ci_l[[t]][1:length(observed_short_domain_l[[t]])], lty="dotted", cex = 0.3, col = "orange")
}
dev.off()


##########################################################################################
## Pooled Mon P SPlines
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "monpspl_pooled_mean.pdf"), height = 10, width = 10) 
par(mai=c(1.2,1.5,1,1))
plot(x = u_m[,1][ !is.na( u_m[,1]) ], y = X_gcv_sp_l[[1]],  
		ylim = c(0, 1300), xlim = c(0,2500), type = "l", pch = 16, cex.axis = 2, cex.lab =2, 
		ylab = "Capacity Price in Euro/MW/w", xlab = "Observed Domain D(t)")
for(t in (2:T)[-32]){
	lines(x = u_m[,t][ !is.na( u_m[,t]) ], y = X_gcv_sp_l[[t]], cex = 0.3, col = "black")
}
abline( v = common_domain_int, col = "red", lwd = 2)
abline( v = range(max_dom_discretization), col = "darkgreen", lty = "dashed", lwd = 2)
lines(	x = obsGrid, y = smcObj$mu, col = "blue",  lwd = 4)
legend(	"topleft", col = c("black", "blue", "red", "darkgreen"), cex = 2, 
						legend = c(expression(paste("Mon. P-Spline ", hat(B)[t])), "Local Linear Mean", "Common Domain", "Maximum Domain"), 
			lwd=c(1,4, 2, 2), lty = c("solid", "solid", "solid", "dashed"))
dev.off()






 
##########################################################################################
## Estimate mean 1st derivative on common intervall:
########################################################################################## 

X_gcv_prime_ci_fit		<- locfit(unlist(X_gcv_prime_ci_l)  ~  lp( rep(com_dom_discretization, times = T), 
														nn = 0.7, ## Nearest neighbor component of the smoothing parameter. Default value is 0.7, unless either h or adpen are provided, in which case the default is 0.
														h = 0, adpen = 0, ## The constant component of the smoothing parameter. Default: 0. Penalty parameter for adaptive fitting.
														deg = 2, ## Degree of polynomial to use.
														acri = "none", ## Criterion for adaptive bandwidth selection.
														scale = FALSE, style = "none" ) )

X_usmth_prime_ci_fit	<- locfit(unlist(X_usmth_prime_ci_l)  ~  lp( rep(com_dom_discretization, times = T), nn = 0.7, h = 0) )
														

X_gcv_prime_ci_mean 	<- predict(X_gcv_prime_ci_fit	, 	newdata = com_dom_discretization)
X_usmth_prime_ci_mean 	<- predict(X_usmth_prime_ci_fit, 	newdata = com_dom_discretization)
														
##########################################################################################
## Estimate mean 1st derivative on sparse intervalls
########################################################################################## 													
X_gcv_prime_sp_fit		<- locfit(unlist(X_gcv_prime_sp_l)  ~  lp( u_v, 
														nn = 0.7, ## Nearest neighbor component of the smoothing parameter. Default value is 0.7, unless either h or adpen are provided, in which case the default is 0.
														h = 0, adpen = 0, ## The constant component of the smoothing parameter. Default: 0. Penalty parameter for adaptive fitting.
														deg = 2, ## Degree of polynomial to use.
														acri = "none", ## Criterion for adaptive bandwidth selection.
														scale = FALSE, style = "none" ) )

X_usmth_prime_sp_fit	<- locfit(unlist(X_usmth_prime_sp_l)  ~  lp( u_v, nn = 0.7, h = 0) )

# include common domain into maximum domain
X_gcv_prime_sp_mean 	<- predict(X_gcv_prime_sp_fit	, newdata = max_dom_discretization)
X_usmth_prime_sp_mean 	<- predict(X_usmth_prime_sp_fit	, newdata = max_dom_discretization)
																												
## Pooled First Derivatives 
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0),"_presmoothed_1stderv_pooled.pdf")) 
plot(x = com_dom_discretization, y = X_gcv_prime_ci_l[[1]], type = "l", pch=16, cex = 0.5, ylim = c(0, 5), col = "lightgray", ylab = "Presmoothed First Derivative")
abline( v = common_domain_int, col = "red")
for(t in 2:T){
	lines(x = com_dom_discretization, y = X_gcv_prime_ci_l[[t]], pch=16, cex = 0.5, col = "lightgray")
}
lines( x = com_dom_discretization, y = X_gcv_prime_ci_mean, cex = 1, col = "blue")
dev.off()
														
##########################################################################################
##########################################################################################
## 3. STEP: Predict 1. Derivative on equidistant 										##
## b) basis on sparse
##########################################################################################
##########################################################################################

													

##########################################################################################
## Plots of pooled functions and 1st derivatives on observed values						##
########################################################################################## 
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcvusmth_presmoothed_0stder_pooled.pdf")) 
plot(x = u_m[,1][ !is.na( u_m[,1]) ], y = X_gcv_sp_l[[1]], type = "l", pch=16, cex = 0.5, ylim = c(0, 1300), xlim=c(0, 2520), ylab = "Presmoothed Function")
for(t in 2:T){
	lines(x = u_m[,t][ !is.na( u_m[,t]) ], y = X_gcv_sp_l[[t]], pch=16, cex = 0.5)
}
# for(t in 1:T){
	# lines(x = u_m[,t][ !is.na( u_m[,t]) ], y = X_under_sp_l[[t]], lty="dotted", cex = 0.3, col = "red")
# }
dev.off()
																						
## Pooled First Derivatives 
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "usmth_presmoothed_1stderv_pooled.pdf")) 
plot(x = u_m[,1][ !is.na( u_m[,1]) ], y = X_usmth_prime_sp_l[[1]], type = "l", pch=16, cex = 0.5, 
		ylim = c(0, 5), xlim = c(0, 2520), col = "lightgray", ylab = "Presmoothed First Derivative")
abline( v = range(u_v), col = "red")
for(t in 2:T){
	lines(x = u_m[,t][ !is.na( u_m[,t]) ], y = X_usmth_prime_sp_l[[t]], pch=16, cex = 0.5, col = "lightgray")
}
lines( x = max_dom_discretization, y = X_usmth_prime_sp_mean, cex = 1, col = "green")
lines( x = max_dom_discretization, y = X_gcv_prime_sp_mean, cex = 1, col = "blue")
abline(h = 0, col = "green")
dev.off()


##########################################################################################
## Application of LieblKneip2015
########################################################################################## 

## Centralizing of observed derivatives:
	
X_monpsp_prime_sp_dm_l <- vector("list", length = T)
X_monpsp_prime_ci_dm_l <- vector("list", length = T)

for(t in 1:T){
	#print(t)
	X_monpsp_prime_sp_dm_l[[t]]<- 	X_monpsp_prime_sp_l[[t]] - predict(X_gcv_prime_sp_fit, newdata = u_m[,t][ !is.na( u_m[,t]) ])
	X_monpsp_prime_ci_dm_l[[t]]<- 	X_monpsp_prime_ci_l[[t]][1:length(observed_short_domain_l[[t]])] - predict(X_gcv_prime_ci_fit, newdata = observed_short_domain_l[[t]])
}

## Centralizing of observed derivatives:
X_gcv_prime_sp_demeaned <- vector("list", length = T)
X_gcv_prime_ci_demeaned <- vector("list", length = T)

for(t in 1:T){
	X_gcv_prime_sp_demeaned[[t]]<- 	X_gcv_prime_sp_l[[t]] - predict(X_gcv_prime_sp_fit, newdata = u_m[,t][ !is.na( u_m[,t]) ])
	X_gcv_prime_ci_demeaned[[t]]<- 	X_gcv_prime_ci_l[[t]] - predict(X_gcv_prime_ci_fit, newdata = com_dom_discretization)
}

# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "monPSpline_presmoothed_1stderv_pooled.pdf")) 
plot(	x = observed_short_domain_l[[1]], y = X_monpsp_prime_ci_l[[1]][1:length(observed_short_domain_l[[1]])], type = "l", pch=16, cex = 0.5, 
		xlim = c(0, 2520), ylim= range(unlist(lapply(X_gcv_prime_sp_demeaned, range))),
		ylab = "Presmoothed First Derivative by monPSpline")
abline( v = range(u_v), col = "red")
for(t in 2:T){
	lines(x = observed_short_domain_l[[t]], y = X_monpsp_prime_ci_l[[t]][1:length(observed_short_domain_l[[t]])], pch=16, cex = 0.5, col = "black")
}

lines( x = max_dom_discretization, y = X_gcv_prime_sp_mean, lwd = 5, col = "blue")
dev.off()


# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_presmoothed_1stderv_pooled.pdf")) 
plot(	x = u_m[,1][ !is.na( u_m[,1]) ], y = X_gcv_prime_sp_demeaned[[1]], type = "l", pch=16, cex = 0.5, 
		xlim = c(0, 2520), ylim= range(unlist(lapply(X_gcv_prime_sp_demeaned, range))),
		ylab = "Presmoothed First Derivative")
abline( v = range(u_v), col = "red")
for(t in 2:T){
	lines(x = u_m[,t][ !is.na( u_m[,t]) ], y = X_gcv_prime_sp_demeaned[[t]], pch=16, cex = 0.5, col = "black")
}
lines( x = max_dom_discretization, y = X_gcv_prime_sp_mean, lwd = 5, col = "blue")
abline( h = 0, col = "green")
dev.off()

## Test: Derivatives at different values observed
#plot( x = max_dom_discretization, y = predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization, nderiv = 1) , type = "l")
#lines(x = u_m[,t][ !is.na( u_m[,t]) ] , y = X_gcv_prime_sp_l[[t]], col ="red")


##########################################################################################
## GCV
## Plot training_index smoothed curves and derivatives
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "training_index_curvesandderivs_withgcv.pdf"), width = 20) 
par(mfrow=c(2,1))
plot( 	x = max_dom_discretization, 
		y = predict(X_gcvsmth_l[[training_index[1]]], xarg = max_dom_discretization), 
		type = "l", ylab = "Training Index Functions", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = max_dom_discretization, 
			y = predict(X_gcvsmth_l[[training_index[t]]], xarg = max_dom_discretization))		
}
plot( 	x = max_dom_discretization, ylim = c(0,4),
		y = predict(X_gcvsmth_l[[training_index[1]]], xarg = max_dom_discretization, nderiv = 1),
		type = "l", ylab = "Training Index Functions", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = max_dom_discretization, 
			y = predict(X_gcvsmth_l[[training_index[t]]], xarg = max_dom_discretization, nderiv = 1))		
}		
abline(h = 0, col = "green")
dev.off()


##########################################################################################
## Monotone P Splines
## Plot training_index smoothed curves and derivatives 
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "training_index_curvesandderivs_withmonpspline.pdf"), width = 20, height = 15) 
par(mfrow=c(2,1))
plot( 	x = observed_short_domain_l[[training_index[1]]], 
		y = X_monpsp_ci_l[[training_index[1]]][1:length(observed_short_domain_l[[training_index[1]]])],
		type = "l", ylab = "Training Index Functions", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = observed_short_domain_l[[training_index[t]]], 
			y = X_monpsp_ci_l[[training_index[ t ]]][1:length(observed_short_domain_l[[training_index[t]]])])		
}
plot( 	x = observed_short_domain_l[[training_index[1]]], 
		y = X_monpsp_prime_ci_l[[training_index[1]]][1:length(observed_short_domain_l[[training_index[1]]])], ylim = c(0,4), 
		type = "l", ylab = "Training Index Derivatives", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = observed_short_domain_l[[training_index[t]]], 
			y = X_monpsp_prime_ci_l[[training_index[t]]][1:length(observed_short_domain_l[[training_index[t]]])])		
}
abline(h = 0, col = "green")
dev.off()

##########################################################################################
##  Difference between Monotone P Splines and GCV Smoothed
## Plot training_index smoothed curves and derivatives 
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "differences_smoothing_trainining_index.pdf"), width = 20, height = 15) 
plot( 	x = max_dom_discretization, ylim = c(0,4),
		y = predict(X_gcvsmth_l[[training_index[1]]], xarg = max_dom_discretization, nderiv = 1),
		type = "l", ylab = "Training Index Functions", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = max_dom_discretization, 
			y = predict(X_gcvsmth_l[[training_index[t]]], xarg = max_dom_discretization, nderiv = 1))		
}		
for(t in 1:length(training_index)){
	lines(	x = observed_short_domain_l[[training_index[t]]], 
			y = X_monpsp_prime_ci_l[[training_index[t]]][1:length(observed_short_domain_l[[training_index[t]]])], col = "green")
}
abline(h = 0, col ="green")
legend(	"topleft", col = c("black", "green"), lty = c("solid","solid"),
			legend = c("GCV", "Mon Pspline"), lwd=1.125)
dev.off()

 ##########################################################################################

 plot( 	x = observed_short_domain_l[[training_index[1]]], 
		y = X_monpsp_ci_l[[training_index[1]]][1:length(observed_short_domain_l[[training_index[1]]])],
		type = "l", ylab = "Training Index Functions", xlab = paste0("Dates: ", dates[training_index[1]], " to ", dates[training_index[length(training_index)]]))
for(t in 2:length(training_index)){
	lines(	x = observed_short_domain_l[[training_index[t]]], 
			y = X_monpsp_ci_l[[training_index[ t ]]][1:length(observed_short_domain_l[[training_index[t]]])])		
}
	
 
##########################################################################################
## Large Domain Training Matrix




X_prime_train_m			<- matrix( NA , nrow = length(max_dom_discretization), ncol = length(training_index))
X_prime_monspl_train_m	<- matrix( NA , nrow = length(max_dom_discretization), ncol = length(training_index))

counter <- 1
for(t in training_index){ # t <- 131, t <- 183
	print(t)
	X_prime_train_m[, counter] 			<- as.vector(predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization, nderiv = 1)) - predict(X_gcv_prime_sp_fit, newdata = max_dom_discretization)
	
	X_prime_monspl_train_m[ , counter] 	<- as.vector(X_monpsp_prime_ci_l[[t]]) - predict(X_monpsp_prime_sp_fit, newdata = max_dom_discretization)
	counter 							<- counter + 1
}

X_prime_monspl_train_m[is.na(X_prime_monspl_train_m)] <- 0


##########################################################################################
## Test for well definition of trainings set; replace neg. derivatives with 0
# if( any(X_prime_train_m < 0) ) X_prime_train_m[X_prime_train_m < 0] <- 0

cov_train_m 			<- ( X_prime_train_m %*% t(X_prime_train_m) ) / length(training_index)

cov_train_monspl_m 		<- ( X_prime_monspl_train_m %*% t(X_prime_monspl_train_m) ) / length(training_index)



## small covariance training matrix on common domain:
X_m						<- matrix(unlist(predict(X_gcvsmth_l, xarg = com_dom_discretization, nderiv = 1)), ncol=T) - matrix(predict(X_gcv_prime_sp_fit, newdata = com_dom_discretization), nrow = length(com_dom_discretization), ncol = T)
cov_small_train_m		<- ( X_m %*% t(X_m) ) / length(com_dom_discretization)


## small covariance training matrix on cutted maximum domain:
cut_train_cov_m				<- getTrainingCovariance(	training_index = training_index, fitted_objects = X_gcvsmth_l, 
														fitted_mean = X_gcv_prime_sp_fit, 
														discretization = max_dom_discretization[max_dom_discretization > start_cutting_point])

## small covariance  matrix on cutted common domain:
X_cut_m						<- matrix(unlist(predict(X_gcvsmth_l, xarg = cut_dom_discretization, nderiv = 1)), ncol=T) - matrix(predict(X_gcv_prime_sp_fit, newdata = cut_dom_discretization), nrow = length(cut_dom_discretization), ncol = T)
cov_small_cut_m				<- ( X_cut_m %*% t(X_cut_m) ) / length(cut_dom_discretization)

## Plot Covariance-Function gamma-hat
##########################################################################################
## Plots of Training Covariance Matrix for GCV
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0), "trainingcovariance.pdf"), height = 20, width = 15)
persp(	x = max_dom_discretization, y = max_dom_discretization, z = cov_train_m,
		main = "", xlab = "MW", ylab = "MW",zlab = "", 
		col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
		ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 3)
dev.off()

# Cuttet Covariance on Training_index
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0), "trainingcovariance_cut.pdf"), height = 20, width = 15)
persp(	x = max_dom_discretization[max_dom_discretization > start_cutting_point], 
		y = max_dom_discretization[max_dom_discretization > start_cutting_point], 
		z = cut_train_cov_m	,
		main = "", xlab = "MW", ylab = "MW",zlab = "", 
		col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
		ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 3)
dev.off()

##########################################################################################
## Plots of Training Covariance Matrix for Mon Spline
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "trainingcovariance_monspline.pdf"), height = 20, width = 15)
persp(	x = max_dom_discretization, y = max_dom_discretization, z = cov_train_monspl_m ,
		main = "", xlab = "MW", ylab = "MW",zlab = "", 
		col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
		ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 3)
dev.off()

##########################################################################################
## Plots of Common Domain Covariance Matrix
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0), "ci_covariance.pdf"), height = 20, width = 15)
persp(	x = com_dom_discretization, y = com_dom_discretization, z = cov_small_train_m, 
		main = "", xlab = "MW", ylab = "MW",zlab = "", 
		col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
		ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 10)
dev.off()

##########################################################################################
## Plots of cutted Common Domain Covariance Matrix
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "gcv_", round(spar_gcv, 0), "ci_covariance_cut.pdf"), height = 20, width = 15)
persp(	x = cut_dom_discretization, y = cut_dom_discretization, z = cov_small_cut_m,
		main = "", xlab = "MW", ylab = "MW",zlab = "", zlim=c(0, 0.05),
		col = gray(1 - hgt), theta = 340, phi = 10, r = 50, d = 0.7, expand = 1, 
		ltheta = 90, lphi = 180, shade = 0.001, ticktype = "detailed", nticks = 10)
dev.off()


##########################################################################################

# normal and dm = demeaned; demeaned are correct ones
# scores calculated with lm
X_prime_recover_3npc_m	 	<- matrix(NA, nrow = length(max_dom_discretization), ncol = T) 
X_prime_recover_3npc_dm_m	<- matrix(NA, nrow = length(max_dom_discretization), ncol = T) 
X_scores_3npc_dm_m	 		<- vector("list", length = T)
X_pr_mons_recover_lm_dm_l	<- vector("list", length = T)

X_prime_recover_2npc_m	 	<- matrix(NA, nrow = length(max_dom_discretization), ncol = T) 
X_prime_recover_2npc_dm_m	<- matrix(NA, nrow = length(max_dom_discretization), ncol = T) 
X_scores_2npc_dm_m	 		<- vector("list", length = T)

X_prime_recover_3cut_m	 	<- matrix(NA, nrow = length(max_dom_discretization[max_dom_discretization > start_cutting_point]), ncol = T) 
X_prime_recover_3cut_dm_m	<- matrix(NA, nrow = length(max_dom_discretization[max_dom_discretization > start_cutting_point]), ncol = T) 
X_scores_3cut_dm_m	 		<- vector("list", length = T)

X_prime_recover_2cut_dm_m	<- matrix(NA, nrow = length(max_dom_discretization[max_dom_discretization > start_cutting_point]), ncol = T) 
X_scores_2cut_dm_m	 		<- vector("list", length = T)

options(warn = 2)

# Recover all other functions t <- 105; example_curves
for( t in (1:T)){ 
	print(t)
	
	## use training curves to fill matrix 
	if ( t %in% training_index){
		X_prime_recover_3npc_dm_m[,t]	<- as.vector(predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization, nderiv = 1)) - predict(X_gcv_prime_sp_fit, newdata = max_dom_discretization)
		X_scores_3npc_dm_m[[t]]			<- 0

		X_prime_recover_2npc_dm_m[,t]	<- as.vector(predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization, nderiv = 1)) - predict(X_gcv_prime_sp_fit, newdata = max_dom_discretization)
		X_scores_2npc_dm_m[[t]]			<- 0
		
		X_prime_recover_3cut_dm_m[,t]	<- as.vector(predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization[max_dom_discretization > start_cutting_point], nderiv = 1)) - predict(X_gcv_prime_sp_fit, newdata = max_dom_discretization[max_dom_discretization > start_cutting_point])
		X_scores_3cut_dm_m[[t]]			<- 0
		# X_pr_mons_recover_lm_dm_l[[t]]	<- as.vector(X_monpsp_prime_ci_l[[t]]) - predict(X_monpsp_prime_ci_fit, newdata = max_dom_discretization)
		X_prime_recover_2cut_dm_m[,t]	<- as.vector(predict(X_gcvsmth_l[[t]], xarg = max_dom_discretization[max_dom_discretization > start_cutting_point], nderiv = 1)) - predict(X_gcv_prime_sp_fit, newdata = max_dom_discretization[max_dom_discretization > start_cutting_point])
		X_scores_2cut_dm_m[[t]]			<- 0
		
		
	} else {
		## match observed short domain with max_dom_discretization
		observed_short_domain  	<- max_dom_discretization[max_dom_discretization <= max(u_m[,t][ !is.na( u_m[,t]) ])]

		## extend observed functions to large domains using the substitution of training covariance matrix given the ci covariance matrix
		prediction_object		<- predictSmalltoBigDomain(	
									cov_train_m 				= cov_train_m, # empirical covariance matrix of trained fully observed functions
									cov_small_train_m			= cov_small_train_m,
									discretization_large_domain = max_dom_discretization,
									observed_short_domain 		= observed_short_domain,
									fitted_object 				= X_gcvsmth_l[[t]], # non_centered object
									fitted_mean					= X_gcv_prime_sp_fit, # fitted mean object; should be able to be predicted
									nderiv 						= 1,
									prediction_of_fitted_object = TRUE,
									substitute_sm_cov_into_large= TRUE,
									n_pc						= 2 ) # number of principal components which should be used
		
		
		
		## extend observed functions to large domains using only the training covariance matrix
		# prediction_object		<- predictSmalltoBigDomain(	
									# cov_train_m 				= cov_train_m, # empirical covariance matrix of trained fully observed functions
									# cov_small_train_m			= FALSE,
									# discretization_large_domain = max_dom_discretization,
									# observed_short_domain 		= observed_short_domain,
									# fitted_object 				= X_gcvsmth_l[[t]], # non_centered object
									# fitted_mean					= X_gcv_prime_sp_fit, # fitted mean object; should be able to be predicted
									# nderiv 						= 1,
									# prediction_of_fitted_object = TRUE,
									# substitute_sm_cov_into_large= FALSE,
									# n_pc						= 3 ) # number of principal components which should be used
									
		
		X_prime_recover_2npc_m[,t]		<- prediction_object[["not_dm"]]
		X_prime_recover_2npc_dm_m[,t]	<- prediction_object[["demeaned"]]
		X_scores_2npc_dm_m[[t]]			<- prediction_object[["scores_train_lm_dm_v"]]

		
		prediction_object		<- predictSmalltoBigDomain(	
									cov_train_m 				= cov_train_m, # empirical covariance matrix of trained fully observed functions
									cov_small_train_m			= cov_small_train_m, # empirical covariance matrix of all functions on small commond domain
									discretization_large_domain = max_dom_discretization,
									observed_short_domain 		= observed_short_domain,
									fitted_object 				= X_gcvsmth_l[[t]], # non_centered object
									fitted_mean					= X_gcv_prime_sp_fit, # fitted mean object; should be able to be predicted
									nderiv 						= 1,
									prediction_of_fitted_object = TRUE,
									substitute_sm_cov_into_large= TRUE,
									n_pc						= 3 )
		
		X_prime_recover_3npc_m[,t]		<- prediction_object[["not_dm"]]
		X_prime_recover_3npc_dm_m[,t]	<- prediction_object[["demeaned"]]
		X_scores_3npc_dm_m[[t]]			<- prediction_object[["scores_train_lm_dm_v"]]
		# prediction_object		<- predictSmalltoBigDomain(	
									# cov_train_m 				= cov_train_monspl_m, # empirical covariance matrix of trained fully observed functions
									# cov_small_train_m			= cov_small_train_m,
									# discretization_large_domain = max_dom_discretization,
									# observed_short_domain 		= observed_short_domain,
									# fitted_object 				= X_monpsp_prime_ci_l[[t]], # non_centered object
									# fitted_mean					= X_monpsp_prime_sp_fit, # fitted mean object; should be able to be predicted
									# nderiv 						= 1,
									# prediction_of_fitted_object = FALSE,
									# substitute_sm_cov_into_large= TRUE,
									# n_pc						= 3 ) # number of principal components which should be used
		
		# X_pr_mons_recover_lm_dm_l[[t]] 	<- prediction_object[["demeaned"]]
		
		
		prediction_object		<- predictSmalltoBigDomain(	
									cov_train_m 				= cut_train_cov_m, # empirical covariance matrix of trained fully observed functions
									cov_small_train_m			= cov_small_cut_m,
									discretization_large_domain = max_dom_discretization[max_dom_discretization > start_cutting_point],
									observed_short_domain 		= observed_short_domain[observed_short_domain > start_cutting_point],
									fitted_object 				= X_gcvsmth_l[[t]], # non_centered object
									fitted_mean					= X_gcv_prime_sp_fit, # fitted mean object; should be able to be predicted
									nderiv 						= 1,
									prediction_of_fitted_object = TRUE,
									substitute_sm_cov_into_large= TRUE,
									n_pc						= 3 )
		
		X_prime_recover_3cut_dm_m[,t] 	<- prediction_object[["demeaned"]]
		X_scores_3cut_dm_m[[t]]			<- prediction_object[["scores_train_lm_dm_v"]]
		
		prediction_object		<- predictSmalltoBigDomain(	
									cov_train_m 				= cut_train_cov_m, # empirical covariance matrix of trained fully observed functions
									cov_small_train_m			= cov_small_cut_m,
									discretization_large_domain = max_dom_discretization[max_dom_discretization > start_cutting_point],
									observed_short_domain 		= observed_short_domain[observed_short_domain > start_cutting_point],
									fitted_object 				= X_gcvsmth_l[[t]], # non_centered object
									fitted_mean					= X_gcv_prime_sp_fit, # fitted mean object; should be able to be predicted
									nderiv 						= 1,
									prediction_of_fitted_object = TRUE,
									substitute_sm_cov_into_large= TRUE,
									n_pc						= 2 )
		
		X_prime_recover_2cut_dm_m[,t] 	<- prediction_object[["demeaned"]]
		X_scores_2cut_dm_m[[t]]			<- prediction_object[["scores_train_lm_dm_v"]]
	}
}



## GCV Extension example_curves
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "ext_der_gcv_subst.pdf"),width = 20, height = 40) 
par(mfrow=c(ceiling(length(example_curves)/2),2))
for(t in example_curves){ # t <- 100 
	plot( 	x = u_m[,t][ !is.na( u_m[,t]) ],	y = X_gcv_prime_sp_demeaned[[t]], type = "l", col = "red", lty = "dotted",
			xlim = range(max_dom_discretization), ylim = c(-.5, .5 ), lwd = 3,
			xlab = paste0("First Demeaned Derivatives on: ", dates[t]), ylab = "Marignal Price in EUR/MW^2")
	abline( h = 0, col = "black")
	abline( v = range(u_m[,t][ !is.na( u_m[,t]) ]), col = "yellow")
	abline( v = range(com_dom_discretization), col = "orange")
	lines(  x = com_dom_discretization, y = X_gcv_prime_ci_demeaned[[t]], col = "red") 
	lines(	x = max_dom_discretization, y = X_prime_recover_lm_dm_m[ ,t], col = "green", pch = 16, cex = 1)
	lines(  x = max_dom_discretization[max_dom_discretization > start_cutting_point],
			y = X_prime_recover_3cut_dm_m[,t], col = "orange", pch = 16, cex = 1)
	legend(	"topleft", col = c("red", "red", "green", "orange"), lty = c("dotted",  "solid","solid", "solid"),
			legend = c("Smth/Demeaned at observed domain","Smth/Demeaned at common sm. domain", "LM Extended", "Cut"), lwd=1.125)
	grid(col = "black")
}
dev.off()

## GCV Extension all curves

for(t in 1:T){ # t <- 14
	pdf(paste0("graphs/singlegraphs/", format(Sys.time(), format="%Y%m%d"), "ext_", t, "_2vs3vscut3npcs.pdf"),width = 15, height = 15) 
	plot( 	x = u_m[,t][ !is.na( u_m[,t]) ],	y = X_gcv_prime_sp_demeaned[[t]], type = "l", col = "red", lty = "dotted",
			xlim = range(max_dom_discretization), ylim = c(-.5, .5 ), lwd = 3,
			xlab = paste0("First Demeaned Derivatives on: ", dates[t]), ylab = "Marignal Price in EUR/MW^2")
	abline( h = 0, col = "black")
	abline( v = range(u_m[,t][ !is.na( u_m[,t]) ]), col = "yellow")
	abline( v = range(com_dom_discretization), col = "orange")
	lines(  x = com_dom_discretization, y = X_gcv_prime_ci_demeaned[[t]], col = "red") 
	lines(	x = max_dom_discretization,	y = X_prime_recover_2npc_m[ ,t], col = "blue")
	lines(	x = max_dom_discretization, y = X_prime_recover_3npc_m[ ,t], col = "green")
	lines(  x = max_dom_discretization[max_dom_discretization > start_cutting_point],
			y = X_prime_recover_3cut_dm_m[,t], col = "pink", pch = 16, cex = 1)
	lines(  x = max_dom_discretization[max_dom_discretization > start_cutting_point],
			y = X_prime_recover_2cut_dm_m[,t], col = "brown", pch = 16, cex = 1)
	legend(	"topleft", col = c("red", "red", "blue", "green", "pink", "brown"), lty = c("dotted",  "solid","solid","solid" ,"solid", "solid"),
			legend = c("Smth/Demeaned at observed domain","Smth/Demeaned at common sm. domain", "LM Extended 2 NPcs", "LM Extended 3 NPcs", "Cut 3 NPCs", "Cut 2 NPCs"), lwd=1.125)
	grid(col = "black")
	dev.off()
}
which(dates == "2011-09-26") # 14
which(dates == "2011-10-31") # 19
which(dates == "2012-10-22") # 70



## MON PSpline Extension
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "ext_der_monpspl_subst.pdf"),width = 20, height = 40) 
par(mfrow=c(ceiling(length(example_curves)/2),2))
for(t in example_curves){ # t <- 14
	plot( 	x = u_m[,t][ !is.na( u_m[,t]) ],	y = X_monpsp_prime_sp_dm_l[[t]], type = "l", col = "red", lty = "dotted",
			xlim = range(max_dom_discretization), ylim = c(-.5, .5 ), lwd = 3,
			xlab = paste0("First Demeaned Derivatives on: ", dates[t]), ylab = "Marignal Price in EUR/MW^2")
	abline( h = 0, col = "black")
	abline( v = range(observed_short_domain_l[[t]]), col = "yellow")
	abline( v = range(com_dom_discretization), col = "orange")
	lines(  x = observed_short_domain_l[[t]], y = X_monpsp_prime_ci_dm_l[[t]], col = "red") 
	lines( 	x = max_dom_discretization, y = X_pr_mons_recover_lm_dm_l[[t]], col = "blue")
	
	legend(	"topleft", col = c("red", "red", "blue"), lty = c("dotted", "solid",  "solid","solid"),
			legend = c("CI: Smth/Demeaned at observed domain","SP: Smth/Demeaned at common sm. domain", "LM Extended"), lwd=1.125)
	grid(col = "black")
}
dev.off()

##########################################################################################


for ( t in rev(training_index)){
	X_scores_2npc_dm_m[[t]] 		<- NULL
	X_scores_3npc_dm_m[[t]] 		<- NULL
	X_scores_3cut_dm_m[[t]] 		<- NULL
	X_scores_2cut_dm_m[[t]] 		<- NULL
}
length(unlist(X_scores_2npc_dm_m))
scores_2npc_m				<-  matrix(unlist(X_scores_2npc_dm_m), ncol = 2 , byrow = TRUE)
scores_3npc_m				<-  matrix(unlist(X_scores_3npc_dm_m), ncol = 3 , byrow = TRUE)
scores_3cut_m				<-  matrix(unlist(X_scores_3cut_dm_m), ncol = 3 , byrow = TRUE)
scores_2cut_m				<-  matrix(unlist(X_scores_2cut_dm_m), ncol = 2 , byrow = TRUE)

# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "scores.pdf"),width = 15, height = 15) 
matplot(cbind(scores_2npc_m, scores_3npc_m, scores_3cut_m), type = "l", col = c("black", "blue", "red", "orange", "yellow", "pink", "green"), lty = 1)
legend(	"bottomright", col = c("black", "blue", "red", "orange", "yellow", "pink", "green"), lty = 1,
		legend = c("1. 2NPC", "2. 2NPC", "1. 3NPC", "2. 3NPC", "3. 3NPC", "1. 2CNPC", "2. 2CNPC"), lwd=1.125)
grid(col = "black")
dev.off()

matplot(scores_3npc_m, type = "l", lty = 1)
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "scores_2npc.pdf"),width = 15, height = 15) 
matplot(scores_2npc_m, type = "l", lty = 1)
legend(	"bottomright", legend = c("1. 2NPC", "2. 2NPC"), lty = 1, col = 1:2)
dev.off()
matplot(scores_3cut_m, type = "l", lty = 1)

matplot(scores_2cut_m, type = "l", lty = 1)
legend(	"bottomright", legend = c("1. 2NPC", "2. 2NPC"), lty = 1, col = 1:2)

save(scores_3npc_m, file = "scores.RData")



##########################################################################################
##########################################################################################
## 4. STEP: PACE																		##
## rho = eigenfunctions																	##
## lambda = eigenvalues																	##
##########################################################################################
##########################################################################################
## Define lists for y_m and u_m
y_l 					<- Mat2List(y_orig_m, 2, na.rm = TRUE)            			
u_l 					<- Mat2List(u_orig_m , 2, na.rm = TRUE)

if(CheckData(y_l, u_l)) print("y_l and u_l did not run through our check!")

# Force the data to be list of numeric members
y_l 					<- lapply(y_l, as.numeric) 
u_l 					<- lapply(u_l, as.numeric)

# Which data type?
IsRegular(u_l)

optns 					<- list(	bwmu = 0, # bandwidth choice for mean function is using CV or GCV -> GCV 
									bwuserCov = 0, # bandwidth choice for covariance function is CV or GCV -> GCV
									selectionMethod = "FVE", # the method of choosing the number of principal components K
									FVEthreshold = 0.995, # the Fraction-of-Variance-Explained
									maxK = min(20, length(y)-1), # maximum number of principal components to consider
									numComponents = NULL, # # maximum number of principal components to return
									dataType = IsRegular(u_l), # do we have dataType or sparse functional data
									kernel = "epan", # kernel: Epanechnikov
									rho = "cv", # truncation threshold for the iterative residual that is used
									verbose = TRUE, # display diagnostic messages
									useBinnedData = "AUTO" # Automated bin data; 'FORCE','AUTO','OFF' possible
								)

								
# Set the options structure members that are still NULL, i.e. complete options
optns 					<- SetOptions(y_l, u_l, optns)

# Check the options validity for the PCA function. 
 numOfCurves 			<- length(y_l);
 if( CheckOptions(t, optns,numOfCurves) ){
    cat('FPCA has stopped.')
    return(FALSE);
 }


 
if(optns$dataType == 'Dense' || optns$dataType == 'DenseWithMV'){
	# Only applicable to Dense and Regular functional data,
	# or Dense data with Missing Values 

	# cross sectional mean and sample covariance for dense case
	# assume no measurement error.
	yu_m 					<- List2Mat(y_l,u_l)

	# Define time grids
	obsGrid 				<- sort(unique(unlist(u_l)))
	regGrid 				<- obsGrid
	workGrid 				<- seq(min(obsGrid), max(obsGrid), length.out = optns$nRegGrid) 

	buff 					<- .Machine$double.eps * max(abs(obsGrid)) * 3 
	minGrid 				<- min(workGrid)
	maxGrid 				<- max(workGrid)
	difGrid 				<- maxGrid - minGrid
	workGrid 				<- workGrid[workGrid > minGrid + difGrid * optns$outPercent[1] - buff & 
					workGrid < minGrid + difGrid * optns$outPercent[2] + buff]
	 
	# get cross sectional mean and sample cov
	smcObj 					<- GetMeanDense(yu_m, optns) # cross sectional mean, for dense data only; plot(smcObj$mu, type = "l")
	mu 						<- smcObj$mu
	smcObj$muDense 			<- ConvertSupport(obsGrid, workGrid, mu = mu)
	scsObj 					<- GetCovDense(yu_m, mu, optns)
	scsObj$smoothCov 		<- ConvertSupport(obsGrid, workGrid, Cov = scsObj$smoothCov)

} else if(optns$dataType == 'Sparse'){
	# For Sparse case
	# Bin the data (potentially):
	if ( optns$useBinnedData != 'OFF'){ 
		BinnedDataset 			<- GetBinnedDataset(y_l,u_l,optns)
		y_l 					<- BinnedDataset$newy
		u_l 					<- BinnedDataset$newt; 
	}

	# Generate basic grids:
	# obsGrid:  the unique sorted pooled time points of the sample and the new data
	# regGrid: the grid of time points for which the smoothed covariance surface assumes values
	obsGrid 				<- sort(unique( c(unlist(u_l))));
	regGrid 				<- seq(min(obsGrid), max(obsGrid),length.out = optns$nRegGrid);


	# Get the smoothed mean curve
	smcObj 					<- GetSmoothedMeanCurve(y_l, u_l, obsGrid, regGrid, optns)


	# Get the smoothed covariance surface
	# mu: the smoothed mean curve evaluated at times 'obsGrid'
	mu 						<- smcObj$mu
	scsObj 					<- GetSmoothedCovarSurface(y_l, u_l, mu, obsGrid, regGrid, optns, optns$useBins) 
	sigma2 					<- scsObj$sigma2

	# workGrid: possibly truncated version of the regGrid; truncation would occur during smoothing
	workGrid 				<- scsObj$outGrid
	} else {
		stop('not implemented for dataType = "p>>n" yet!')
	}

	# Get the results for the eigen-analysis; phi = eigenfunctions
	eigObj 					<- GetEigenAnalysisResults(smoothCov = scsObj$smoothCov, workGrid, optns)

	# Truncated obsGrid, and observations. Empty observation due to truncation has length 0.
	truncObsGrid 			<- obsGrid
	if (!all(abs(optns$outPercent - c(0, 1)) < .Machine$double.eps * 2)) {
		buff 					<- .Machine$double.eps * max(abs(truncObsGrid)) * 3
		truncObsGrid 			<- truncObsGrid[truncObsGrid >= min(workGrid) - buff & truncObsGrid <= max(workGrid) + buff]
		tmp 					<- TruncateObs(y_l, u_l, truncObsGrid)
		y_l 					<- tmp$y_l
		u_l 					<- tmp$u_l
	}

	# convert phi and fittedCov to obsGrid.
	muObs 					<- ConvertSupport(obsGrid, truncObsGrid, mu=mu)
	phiObs		 			<- ConvertSupport(workGrid, truncObsGrid, phi=eigObj$phi)
	CovObs 					<- ConvertSupport(workGrid, truncObsGrid, Cov=eigObj$fittedCov)

	# Get scores  = rho
	if (optns$methodXi == 'CE') {
		if (optns$rho != 'no') { 
			if( length(y_l) > 2048 ){
				randIndx 					<- sample( length(y_l), 2048)
				rho 						<- GetRho(y_l[randIndx], u_l[randIndx], optns, muObs, truncObsGrid, CovObs, eigObj$lambda, phiObs, sigma2)
			} else {
				rho 						<- GetRho(y_l, u_l, optns, muObs, truncObsGrid, CovObs, eigObj$lambda, phiObs, sigma2)
			}
		sigma2 <- rho
		}
		scoresObj 				<- GetCEScores(y_l, u_l, optns, muObs, truncObsGrid, CovObs, eigObj$lambda, phiObs, sigma2)
	} else if (optns$methodXi == 'IN') {
		ymat 				<- List2Mat(y_l,u_l)
		scoresObj 			<- GetINScores(ymat, u_l, optns, muObs, eigObj$lambda, phiObs)
	}

	if (optns$fitEigenValues) {
		fitLambda 			<- FitEigenValues(scsObj$rcov, workGrid, eigObj$phi, optns$numComponents)
	} else {
		fitLambda 			<- NULL
	}
# Make the return object by MakeResultFPCA
FPCA_l 				<- MakeResultFPCA(optns, smcObj, muObs, scsObj, eigObj, scoresObj, truncObsGrid, workGrid, rho=ifelse(optns$rho =='cv', rho, NA), fitLambda=fitLambda)
str(FPCA_l, max = 1)

		
 # Make a quick diagnostics plot  
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "pace_diagnostics.pdf")) 
diagnostics_plot <- createDiagnosticsPlot(u_l , FPCA_l, openNewDev = T)
dev.off()
  
createCovPlot
createCovPlot(FPCA_l, corrPlotType = "Fitted", isInteractive = TRUE)
createCovPlot(FPCA_l, corrPlotType = "Raw", isInteractive = FALSE)
createCovPlot(FPCA_l, corrPlotType = "Smoothed", isInteractive = FALSE)
createDesignPlot(u_l, obsGrid = obsGrid, isColorPlot = TRUE, noDiagonal = TRUE, yname = NULL)
createScreePlot(FPCA_l$FVE) 

##########################################################################################
##########################################################################################
## 4. STEP: Pool Diff Quots																##
##########################################################################################
##########################################################################################


# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "biddata_pooled.pdf")) 
plot(x = u_v, y = na.omit(y_v), type = "p", pch=16, cex = 0.5)


dev.off()

## 1. Version: Discrete difference quotients: empty object for different quotients
diff_quot_l				<- vector("list",T) 
for_quot_l 				<- vector("list",T)
back_quot_l 			<- vector("list",T)
cent_quot_l				<- vector("list",T)


## Definiton of the quotients and of the sorted quotients
for( i in 1:T){
	print(paste0("i: ", i, "; Length of y_l: ", length(y_l[[ i ]])))
	diff_quot_l[[ i ]] 			<- difference_quotients(y_l[[ i ]], u_l[[ i ]])	
	for_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 1 ]]
	back_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 2 ]]
	cent_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 3 ]]
}


ylim 					<- NULL
ylim					<- c(0,10)

# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquots_pooled.pdf")) 
par(mar=c(5.1, 5.1, 4.1, 5.1), mfrow=c(3,1))
plot(x = u_v, y = unlist(for_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Forward Quotient" , ylim = ylim)
plot(x = u_v, y = unlist(back_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Backward Quotient" , ylim = ylim)
plot(x = u_v, y = unlist(cent_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Centered Quotient" , ylim = ylim)
dev.off()

## Smoothing of the pooled diffquots_pooled
for_quot_sort_v			<- unlist(for_quot_l)[u_order_v]
back_quot_sort_v		<- unlist(back_quot_l)[u_order_v]
cent_quot_sort_v		<- unlist(cent_quot_l)[u_order_v]
						


for_quot_fit 			<- locfit(for_quot_sort_v  ~  lp( u_sort_v  , nn = 0.7, h = 0 ) )
for_quot_hat_pred  		<- predict(for_quot_fit, newdata = grid_list[[1]])

back_quot_fit 			<- locfit(back_quot_sort_v  ~  lp( u_sort_v  , nn = 0.7, h = 0 ) )
back_quot_hat_pred  	<- predict(back_quot_fit, newdata = grid_list[[1]])

cent_quot_fit 			<- locfit(cent_quot_sort_v  ~  lp( u_sort_v  , nn = 0.7, h = 0 ) )
cent_quot_hat_pred  	<- predict(cent_quot_fit , newdata = grid_list[[1]])



#################################################################################################		
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquots_pooled_w_loclinfit.pdf"), height = 15) 
par(mar=c(5.1, 5.1, 4.1, 5.1), mfrow=c(5,1))
plot(	x = u_v, y = unlist(for_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Forward Quotient" , ylim = ylim)
lines(	x = grid_list[[1]], y = for_quot_hat_pred, type = "l", col = "red") 
plot(	x = u_v, y = unlist(back_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Backward Quotient" , ylim = ylim)
lines(	x = grid_list[[1]], y = back_quot_hat_pred, type = "l", col = "green") 
plot(	x = u_v, y = unlist(cent_quot_l), type = "p", pch=16, cex = 0.5, ylab = "Centered Quotient" , ylim = ylim)
lines(	x = grid_list[[1]], y = cent_quot_hat_pred, type = "l", col = "blue") 
plot(	x = u_v, y = unlist(X_monpsp_prime_sp_l), type = "p", pch=16, cex = 0.5, ylab = "monot. PSpline" , ylim = ylim)
lines(	x = grid_list[[1]], y = X_monpsp_prime_sp_pred, type = "l", col = "yellow") 
plot(	x = grid_list[[1]], y = for_quot_hat_pred, type = "l", col = "red") 
lines(	x = grid_list[[1]], y = back_quot_hat_pred, type = "l", col = "green") 
lines(	x = grid_list[[1]], y = cent_quot_hat_pred, type = "l", col = "blue") 
lines(	x = grid_list[[1]], y = X_monpsp_prime_sp_pred, type = "l", col = "yellow") 
lines(	x =  grid_list[[1]], y = X_monpsp_prime_ci_pred, type = "l", col = "pink") 
legend("topleft",	col = c("red", "green", "blue",  "yellow","pink"),
					legend = c("Forward", "Backward", "Centered", "MonPSpl SP", "MonPSpl CI"),lwd=1.125)
grid(col = "black")
dev.off()


## Difference with presmoothed mean from 4.STEP
## pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "Different_mean_1stderivatives.pdf"), width = 15, height = 15) 
plot(	x = grid_list[[1]], y = for_quot_hat_pred, type = "l", col = "red", ylim = c(0,1)) 
lines(	x = grid_list[[1]], y = back_quot_hat_pred, type = "l", col = "green") 
lines(	x = grid_list[[1]], y = cent_quot_hat_pred, type = "l", col = "blue")
lines( x = max_dom_discretization, y = X_gcv_prime_sp_mean, cex = 2, col = "orange")
lines( x = max_dom_discretization, y = X_usmth_prime_sp_mean, cex = 2, col = "pink")
lines( x = grid_list[[1]], y = X_monpsp_prime_sp_pred, cex = 2, col ="yellow")
lines(	x = grid_list[[1]], y = X_monpsp_prime_ci_pred, type = "l", col = "darkgreen") 
legend("topleft",	col = c("red", "green", "blue", "orange", "pink", "yellow", "darkgreen"),
					legend = c("Forward", "Backward", "Centered", "GCV Presmoothed", "Undersmoothed", "monotPSpline", "monPSpline CI"),lwd=1.125)
grid(col = "black")
dev.off()

## From Difference Quotients back to Function: dy/dx = (f(x + dx) - f(x)) / dx  ->  dy/dx * dx + f(x) = f(x+dx)
## Mean function coming from difference quotients

## Definition of Diffquotmean: min(y) + cumsum(diff_quot * bin)
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquotmean_bymin_to_example_curves.pdf"),width = 20) 
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))
bin_step				<- mean(diff(grid_list[[1]]))
for(i in example_curves){
	mean_data 				<- data.frame(	x = c(0, plot_grid), 
											y = cumsum(c(data_l$quantiles[i , "min"], min(grid_list[[1]]) * cent_quot_hat_pred[1], bin_step * cent_quot_hat_pred[-1])))
											
	plot(mean_data, ylim=c(0, 710), type = "l", col = "red")
	lines( x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black")
	points(x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black", pch = 16, cex = 0.5)
}
dev.off()


## Definition of Diffquotmean: cumsum(diff_quot * bin) + mean_difference(cumsum, data_mean)
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquotmean_bymean_to_example_curves.pdf"),width = 20) 
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))
bin_step				<- mean(diff(grid_list[[1]]))
for(i in example_curves){
	y 						<- cumsum(c( 0, min(grid_list[[1]]) * cent_quot_hat_pred[1], bin_step * cent_quot_hat_pred[-1])) 
	data_mean				<- mean(data_l$price[, i ], na.rm = TRUE) 
	diff_means				<- data_mean - mean(y)

	mean_data 				<- data.frame(	x = c(0, plot_grid), 
											y = y + diff_means)
											
	plot(mean_data, ylim=c(0, 710), type = "l", col = "red")
	lines( x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black")
	points(x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black", pch = 16, cex = 0.5)
}
dev.off()

## Definition of Diffquotmean: cumsum(diff_quot * bin) + mean_difference(cumsum, data_wmean)
# pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquotmean_bywmean_to_example_curves.pdf") ,width = 20) 
par(mfrow=c(ceiling(length(example_curves)/2),2))
plots 					<- vector("list", length = length(example_curves))
bin_step				<- mean(diff(grid_list[[1]]))
for(i in example_curves){
	y 						<- cumsum(c( 0, min(grid_list[[1]]) * cent_quot_hat_pred[1], bin_step * cent_quot_hat_pred[-1])) 
	data_wmean				<- data_l$quantiles$gavg[i] 
	
	# force mean_function to have same demand descriptive	
	demand_index			<- grid_list[[1]] < max(data_l$mws[ ,i], na.rm = TRUE) 							
	diff_wmeans				<- data_mean - weighted.mean( y[demand_index], w = c(0, min(grid_list[[1]]), rep(bin_step, times = (length(y[demand_index])-2))))

	mean_data 				<- data.frame(	x = c(0, plot_grid), 
											y = y + diff_wmeans)
											
	plot(mean_data, ylim=c(0, 710), type = "l", col = "red")
	lines( x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black")
	points(x = na.omit(data_l$mws[, i ]), y = na.omit(data_l$price[, i ]), col = "black", pch = 16, cex = 0.5)
}
dev.off()

##########################################################################################
## Bin Plots for Diff Quotients	     													##
##########################################################################################
## Bin Plot depending on lp Resolution (Intervalls = 2*lpResolution, mwLevelSeq for coloring; maximum 9 Intervalls)
## bidData of the structure from before with dates as bidData$dates and lps as bidData$lps
## color has to be something of colorbrewer
## in case of weeks

## types: type="bin", type="bidSize", type="mwPosition" 
mwLevelSeq 				<- c(seq(0,180,30),3000)
Xticks 					<- Get_xTicks(data_l$quantiles$dates)
# hightlight christmas demand change:
dates_christmas_df 		<- as.data.frame(cbind(	event = c("Christmas 2013", "Christmas 2014"), 
												startDate = c("2013-12-21", "2014-12-20"), #two days before first acution
												endDate = c("2014-01-01", "2014-12-31")), # four days after second auction
												rownames = NULL, stringsAsFactors = FALSE)
dates_christmas_df$startDate <- as.Date(dates_christmas_df$startDate)
dates_christmas_df$endDate 	<- as.Date(dates_christmas_df$endDate)


diff_quot_res			<- 0.05 # resolution for bins
				
for(pm in PM){

  for(htnt in HTNT){
    x         				<- get(paste0("biddata_", pm, "_",htnt)) # bidData
    
	
	y_l 					<- Mat2List(x$price, 2, na.rm = TRUE)            			
	u_l						<- Mat2List(x$mws, 2, na.rm = TRUE)

	y_v 					<- unlist(y_l)
	u_v 					<- unlist(u_l)
	
	## empty object for different quotients
	diff_quot_l				<- vector("list",T) 
	for_quot_l 				<- vector("list",T)
	back_quot_l 			<- vector("list",T)
	cent_quot_l				<- vector("list",T)
	for_quot_m				<- matrix(NA, nrow = dim(x$price)[1], ncol = dim(x$price)[2])
	back_quot_m 			<- matrix(NA, nrow = dim(x$price)[1], ncol = dim(x$price)[2])
	cent_quot_m				<- matrix(NA, nrow = dim(x$price)[1], ncol = dim(x$price)[2])
	
	
	## Definiton of the quotients and of the sorted quotients
	for( i in 1:T){
		print(paste0("i: ", i, "; Length of y_l: ", length(y_l[[ i ]])))
		diff_quot_l[[ i ]] 			<- difference_quotients(y_l[[ i ]], u_l[[ i ]])	
		n_diffquots					<- length(diff_quot_l[[ i ]][[ 1 ]])
		for_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 1 ]]
		back_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 2 ]]
		cent_quot_l[[ i ]]			<- diff_quot_l[[ i ]][[ 3 ]]
		
		for_quot_m[ (1:n_diffquots) , i]			<- diff_quot_l[[ i ]][[ 1 ]]
		back_quot_m[ (1:n_diffquots) , i]			<- diff_quot_l[[ i ]][[ 2 ]]
		cent_quot_m[ (1:n_diffquots) , i]			<- diff_quot_l[[ i ]][[ 3 ]]
	}
	
	max_price    			<- max(sapply(cent_quot_l, max, na.rm=TRUE))
	max_price 				<- 8
	x$quantiles$gavg		<- sapply(cent_quot_l, mean, na.rm = TRUE)
	x$quantiles$max			<- sapply(cent_quot_l, max, na.rm = TRUE)
	diffquot_data_l 		<- list(quantiles = x$quantiles, bidSizes = x$bidSizes, dates = x$dates, 
									gavgh = x$quantiles$gavg, 
									price = cent_quot_m)

	
    Xticks    				<- Get_xTicks(x$quantiles$dates)
    filename  				<- paste0(rla, "_", pm, "_",htnt)
    cust      				<- customizeProduct(filename) # customize colours

    assign( 	x = 	paste0("diffquot_binplot_", pm, "_",htnt), 
				value = getPlot(diff_quot_res, mwLevelSeq, bidData = diffquot_data_l, color=cust$color[1], type="bin", 
								filename = filename, pricetype, max_price = max_price,
								rev=as.logical(cust$color[2]), pch=15, pointSize=1, h=F, ymax = max_price) ) 
		
  }
} # now four objects for each combinations of pm and htnt, e.g.:

pdf(paste0("graphs/", format(Sys.time(), format="%Y%m%d"), "diffquot_bin_plot_all.pdf"), width = 20)
grid.arrange(	diffquot_binplot_POS_HT, diffquot_binplot_POS_NT, 
				diffquot_binplot_NEG_HT, diffquot_binplot_NEG_NT, nrow=2)
dev.off()
  



