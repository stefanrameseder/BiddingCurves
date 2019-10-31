

modalDepth <- function( curvedata , com_grid , bandwidth = NULL) {

    ## Modal Depth according to Febrero-Bande (2007)

    ### INPUT:
    ## - curvedata: a matrix with N functional observations, each row per observation
    ## - com_grid: T argument values, where functions are evaluated
    ## - bandwidth: optional bandwidth

    ### OUTPUT:
    ## - depthMD: vector of length N where each entry is the i'th curve's modal depth

    N  <- dim(curvedata)[1]
    T  <- dim(curvedata)[2]
    deltas <- com_grid[2:T] - com_grid[1:T-1]

    ## gaussian kernel
    gKernel <- function(t) {
        2/sqrt(2*pi) * exp( -0.5 * t^2 )
    }

    ## calculate all pairwise norms and fill N x N matrix
    normJK <- function(pair) {
        j <- pair[1]
        k <- pair[2]
        sum( deltas * sqrt( ( curvedata[j,2:T] - curvedata[k,2:T] )^2 ))
    }
    norms      <- combn( 1:N, 2, normJK) ## vectorized upper triangular
    norm_mat   <- matrix(0, ncol=N, nrow=N)
    end        <- 0
    for (i in 1:(N-1)) {
        start                 <- end + 1
        end                   <- start + N - i - 1
        norm_mat[ i, (i+1):N] <-  norms[start:end]
    }
    norm_mat   <- norm_mat + t( norm_mat) ## full matrix
    
    if (is.null(bandwidth)) {
        ## then calculate bandwidth as the 15th
        ## percentile of 'norms'
        bandwidth  <- quantile( c( norms, rep(0,N)) , 0.15)
    }

    depthMD <- colSums( gKernel( norm_mat * 1/bandwidth))
    return(depthMD)
}
