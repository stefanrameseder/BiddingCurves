#############################################
## ConvertSupport
ConvertSupport <- function (fromGrid, toGrid, mu = NULL, Cov = NULL, phi = NULL) 
{
	# Input:
	# - fromGrid: which grid should be started at?
	# - toGrid: 
    buff <- .Machine$double.eps * max(abs(fromGrid)) * 3
    if (abs(toGrid[1] - fromGrid[1]) < buff) 
        toGrid[1] <- fromGrid[1]
    if (abs(toGrid[length(toGrid)] - fromGrid[length(fromGrid)]) < 
        buff) 
        toGrid[length(toGrid)] <- fromGrid[length(fromGrid)]
    if (!is.null(mu)) {
        return(mapX1d(fromGrid, mu, toGrid))
    }
    else if (!is.null(Cov)) {
        gd <- pracma::meshgrid(toGrid)
        ret <- matrix(interp2lin(fromGrid, fromGrid, Cov, gd$X, 
            gd$Y), nrow = length(toGrid))
        ret <- 0.5 * (ret + t(ret))
        return(ret)
    }
    else if (!is.null(phi)) {
        return(mapX1d(fromGrid, phi, toGrid))
    }
}
