
normalityTestFunction <- function(sample){
    ## Center data
    redDomSample_centered <- sample - rowMeans(sample)
    
    
    ## PCA onto reduced sample
    pca    	<- prcomp(t(redDomSample_centered))
    
    ## Adjust Eigenvectors/Eigenfunctions
    evecs  	<- pca$rotation * sqrt(dim(sample)[1]) # adjustment (approx) in order to have length 1 in L^2
    
    # First three eigenvectors
    #matplot(evecs[,1:3], type = "l")
    
    # Choose according to cum variance
    evals  	<- pca$sdev^2
    cpv    	<- sapply(1:length(evals), FUN=function(k) return( sum(evals[1:k])/sum(evals)) )
    print(cpv[1:4])
    print(paste0("Number of p's:", dim(sample)[1]))
    print(paste0("Number of n's:", dim(sample)[2]))
    evec        <- evecs[ ,1:2] 
    scores 		<- t(redDomSample_centered) %*% evec * 1/(dim(sample)[1])
    print(shapiro.test(scores[,1]))
    hist(scores[,1], prob=TRUE, breaks = 40)  #?hist          # prob=TRUE for probabilities not counts
    lines(density(scores[,1]))  
    curve(dnorm(x,mean=mean(scores[,1]), sd=sd(scores[,1])), col = "red", add = TRUE)
    return(scores = scores[,1])
}
