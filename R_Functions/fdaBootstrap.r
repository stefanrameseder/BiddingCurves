
fdaBootstrap <- function( curvedata, com_grid , B=200 , n=NULL , alpha=NULL , gamma=0.05 , depthtype="FMD", computeC = FALSE ){
    ## bootstrap n curves with zero mean from sample curvedata
    ## according to Febrero-Bande (2007)
    ## and compute depth threshold C for outlier

    ## INPUT:
    ## - curvedata: N x T matrix of evaluated curves at com_grid
    ## - com_grid:  vector of length T, common grid for evaluation
    ## - B:         bootstrap replication
    ## - n:         how many curves shall be generated per replication?
    ## - alpha:     trimming parameter, takes values in [0,1]
    ## - gamma:     bootstrapping parameter
    ## - depthtype: string, either MD or FMD

    ## OUTPUT:
    ## - bstrCurves: n x T matrix, bootstrapped curves, evaluated at com_grid
    ## - or if computeC = TRUE: C for outlier
    if (is.null( alpha)) alpha <- 0.05
    N      <- dim(curvedata)[1]
    T      <- dim(curvedata)[2]


    ## compute depth (step 1)

    if (depthtype=="MD"){
        depths <- modalDepth(curvedata, com_grid)
    } else if (depthtype=="FMD"){
        depths <- fmDepth(curvedata, com_grid)
    } else {
        stop("depth function can be either MD or FMD")
    }
    

    ## remove alpha least depth curves
    nRemove <- round( alpha * N , 0) + 1
    keep    <- order(depths)[nRemove:N]
    kpSmpl  <- curvedata[keep,]
    cSmpl   <- t( t(kpSmpl) - colMeans(kpSmpl))
    if (is.null(n))  n  <- N
    
    ## FPCA:
    ## take PC's that explain 99% (5 at least)
    covMat  <- cov(cSmpl)
    eDec    <- eigen( covMat)
    eigVals <- eDec$values
    ratio   <- 0
    cntr    <- 1
    while (ratio<0.99) {
        ratio <- sum(eigVals[1:cntr])/sum(eigVals)
        cntr  <- cntr + 1
    }
    nBase   <- max(5 , cntr)
    eigFu   <- eDec$vectors[,1:nBase]
    scores  <- kpSmpl %*% eigFu

    ## main bootstrapping procedure
    means <- colMeans(scores)
    sds   <- apply(scores, 2, sd)
    idx   <- 1:dim(kpSmpl)[1] # index to select from

    bootstr <- function(k){
        ## first select n curves from kpSmpl (step 2)
        sel       <- sample( idx , n , replace =TRUE)
        sampleI   <- kpSmpl[sel,]
        ## then generate equalle distributed curves and add them (step 3)
        bsScore   <- matrix( unlist(lapply( 1:nBase ,function(i) return( rnorm( n , mean=means[i] , sd=sds[i] )) )),ncol=n , byrow=TRUE )
        outCurves <- sampleI + t( sqrt(gamma) *  eigFu %*% bsScore )
        return(outCurves)
    }
    ## dram B samples
    fullSample_l <- lapply(1:B , bootstr )

    if (computeC) {

        ## compute percentile of depths for each sample (step 4)
        firstPctile  <- function(sample){
            if (depthtype=="MD"){
                depths <- modalDepth( sample, com_grid)
            } else {
                depths <- fmDepth( sample, com_grid)
            }
            return( quantile(depths , probs = 0.01 ))
        }

        ## takes some time...
        depthPercentile <- unlist( lapply( fullSample_l , firstPctile ))
        C_val <- quantile( depthPercentile , 0.5)
        ## return the median of all depths (step 5)
        ret   <- C_val
    } else {
        ret <- fullSample_l
    }
    return(ret )
}

