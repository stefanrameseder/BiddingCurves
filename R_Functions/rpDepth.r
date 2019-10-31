

rpDepth <- function( curvedata , curvedata_deriv , com_grid) {
    ## Computes Random Direction Depth
    ## INPUT:
    ## - curvedata: N x T matrix, one obs per row
    ## - curvedata_deriv: same as curvedata but derivatives
    ## - com_grid: grid, where curves and derivs are evaluated

    ## OUTPUT:
    ## - rpDepth: vector of length N, depth for each curve

    N      <- dim(curvedata)[1]
    T      <- dim(curvedata)[2]
    deltas <- com_grid[2:T] - com_grid[1:T-1]
    ## Generate N random directions of length T
    
    rd <- function(n){
        rdirect <- rnorm(n)
        return( rdirect * 1/ sqrt( sum( rdirect^2)) ) 
    }
    
    R_m       <- matrix(  rd(T*N), ncol=N)
    valproj   <- curvedata[,2:T] %*% ( R_m[2:T,] * deltas )
    derivproj <- curvedata_deriv[,2:T] %*% ( R_m[2:T,] * deltas )
    
    proj      <- lapply(1:N , function(i) cbind( valproj[,i] , derivproj[,i]) )
    
    ## gaussian kernel
    gKernel <- function(t) {
        2/sqrt(2*pi) * exp( -0.5 * t^2 )
    }
        
    ## distance between j , k
    normJK <- function(pair, cdata) {
        j <- pair[1]
        k <- pair[2]
        sum(sqrt( (cdata[j,]-  cdata[k,])^2) )
    }
    ## modal depth
    mdFU      <- function( cdata) {
        norms      <- combn( 1:N, 2, normJK , cdata = cdata) ## vectorized upper triangular
        norm_mat   <- matrix(0, ncol=N, nrow=N)
        end        <- 0
        for (i in 1:(N-1)) {
            start                 <- end + 1
            end                   <- start + N - i - 1
            norm_mat[ i, (i+1):N] <-  norms[start:end]
        }
        norm_mat   <- norm_mat + t( norm_mat) ## full matrix
        bandwidth  <- quantile( c( norms, rep(0,N)) , 0.15)
        depthMD    <- colSums( gKernel( norm_mat * 1/bandwidth))
        depth      <- sum(depthMD) * 1/N
        return(depth)
    }
    ## takes some time
    modDepths <- unlist( lapply(proj ,  mdFU) )
    return(modDepths)
}

