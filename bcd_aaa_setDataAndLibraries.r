## ###########################################
### Used Libraries

rm(list=ls())
require(parallel) # for parallel spline smoothing
require(MASS) # for df


require(devtools) # to install older packages
library(pkgbuild)
find_rtools()
# install_version("coneproj", version = "1.11", repos = "http://cran.us.r-project.org") # cone projections for smoothing parameter lambda
library("coneproj")
packageVersion("coneproj") # if not 1.11., monot splines might not work
require(orthogonalsplinebasis) # cone projections for smoothing parameter lambda
require(locfit) # for local linear fit with alternative means

# For Graphics, in particular get_step_curve
require(ggplot2) # graphs, in particular plots
require(grid) # for multiplot function
#library(dplyr) # like plyr a package for dataset operators
library("scales")
library("reshape")
library("scatterplot3d")
library(RColorBrewer)

# For FDA-Analysis
require(fda.usc) # Outlier Analysis, FDA objects, ...
require(far) # Functional Autoregression for FAR(1) models
library("fda")    # convenient basis functions
library("scales") # colors

# For Time Series
library("timeDate")
library("tseries") # install.packages("fda")
library(zoo)
library("dynlm")
library(xts)
library(forecast) # install.packages("forecast") # remove.packages("forecast")
library("functional") # install.packages("functional")
library(GenSA) # install.packages("GenSA")

library(pbapply) # progressbar fors lapply install.packages("pbapply")
library(timeSeries) # fast Plots install.packages("timeSeries")
library("prophet")

loadedLibraries <- search()[2:(length(search())-2)]


##########################################################################################
### Source functions
setwd("C:\\Users\\stefan.rameseder\\Documents\\BiddingCurves")

function_path   <- "R_Functions"
for(file in list.files(function_path)){
    source(paste0(function_path, "/", file))
    print(paste0(file, " was sourced"))
}
loadedFunctions <- ls()[ls() != "loadedLibraries"]

##########################################################################################
### Setup for variables


# Products ###############################################################################
PM   				  			<- c("POS", "NEG")
HTNT 				  			<- c("HT", "NT")
PMHTNT							<- c("POS_HT", "POS_NT", "NEG_HT", "NEG_NT")
rla            					<- "SRL"
PriceTypes      				<- c("LP", "AP") # "LP" or "AP"
pmhtnt 							<- PMHTNT[1]
pricetype 						<- PriceTypes[1]
########################################################################################## 
