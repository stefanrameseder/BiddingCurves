# rm(list=ls())

# user     	<- "stefan" 		# User Choice for Working Directory

# if(user == "stefan"){
    # if(Sys.info()[1]=="Darwin") {
        # setwd("/Users/stefanrameseder/Google Drive/ADW/R_Code")
    # } else {
        # setwd("C:/Users/LocalAdmin/Google Drive/ADW/R_Code")
    # }
# }


# Source Simulation DGP Data for DGP_name 
# source("bcd_setDataAndLibraries.R"))



## Generate Functional Data:
n        	<- 100 # Number of functions
T        	<- 200 # Number of evaluation points per function
tt       	<- seq(0,1,len=T) # Seq. of evaluation points
nbasis   	<- 5   # Number of basis functions
seq.ev   	<- rev(seq(from=2,to=.5,len=nbasis)) # Seq. of eigenvalues

## Choice of Mean Function:
true_mean	<- Vectorize(function(x) 3*sin(3*x) + cos(5*x))

plot(tt, true_mean(tt), type = "l")


## Creating a basis-object (check also the other possible basis functions!)
X_B      	<- create.bspline.basis(rangeval = c(0,1), nbasis = nbasis)

## Simulating scores
scrs_mat 	<- matrix(NA, nrow=nbasis, ncol=n)
for(b in 1:nbasis){
  scrs_mat[b,] 	<- rnorm(n,sd=sqrt(seq.ev[b]))
}
(eval.basis(evalarg = tt, basisobj = X_B) %*% scrs_mat)[1:2, 1:3]

## Simulated functions
X_mat    	<- (eval.basis(evalarg = tt, basisobj = X_B) %*% scrs_mat) + matrix(true_mean(tt), ncol = n, nrow = T)
dim(X_mat)

matplot(X_mat, x=tt, lty=1, col=add.alpha("black",alpha = .3), type="l", xlab="t")
lines(tt, true_mean(tt), col = "green", lwd = 2)
lines(rowMeans(X_mat), x=tt, lty=3, col = add.alpha("blue",alpha = .5), lwd=3)
legend(	"topright", legend = c("X_t", "True mean", "Estimated mean"),
		lty = c(1,1,3), 
		col = c(add.alpha("black",alpha = .3), "green", add.alpha("blue",alpha = .5)),
		lwd = c(1,2,3),
		title = "Sample, true and estimated mean")

