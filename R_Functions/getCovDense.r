
GetCovDense <- function (ymat, mu, optns) 
{
    if (!(optns$dataType %in% c("Dense", "DenseWithMV"))) {
        stop("Sample Covariance is only applicable for option: dataType = \"Dense\" or \"DenseWithMV\"!")
    }
    n = nrow(ymat)
    K = cov(ymat, use = "pairwise.complete.obs")
    K = 0.5 * (K + t(K))
    ret = list(rawCov = NULL, smoothCov = K, bwCov = NULL, sigma2 = NULL, 
        outGrid = NULL)
    class(ret) = "SmoothCov"
    gc()
    return(ret)
}
#############################################
## 