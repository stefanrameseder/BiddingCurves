


GetSmoothedCovarSurface <- function (y, t, mu, obsGrid, regGrid, optns, useBins = FALSE) 
{
    dataType <- optns$dataType
    error <- optns$error
    kern <- optns$kernel
    bwuserCov <- optns$bwuserCov
    bwuserCovGcv <- optns$bwuserCovGcv
    verbose <- optns$verbose
    rotationCut <- optns$rotationCut
    outPercent <- optns$outPercent
    buff <- .Machine$double.eps * max(abs(obsGrid)) * 3
    rangeGrid <- range(regGrid)
    minGrid <- rangeGrid[1]
    maxGrid <- rangeGrid[2]
    cutRegGrid <- regGrid[regGrid > minGrid + diff(rangeGrid) * 
        outPercent[1] - buff & regGrid < minGrid + diff(rangeGrid) * 
        outPercent[2] + buff]
    rcov <- GetRawCov(y, t, obsGrid, mu, dataType, error)
    if (useBins && bwuserCovGcv == "CV") {
        stop("If bwuserCovGcv == 'CV' then we must use the unbinned rcov.")
    }
    if (useBins) {
        rcov <- BinRawCov(rcov)
    }
    if (bwuserCov == 0) {
        if (bwuserCovGcv %in% c("GCV", "GMeanAndGCV")) {
            gcvObj <- gcvlwls2dV2(obsGrid, regGrid, kern = kern, 
                rcov = rcov, verbose = verbose, t = t)
            bwCov <- gcvObj$h
            if (bwuserCovGcv == "GMeanAndGCV") {
                bwCov <- sqrt(bwCov * gcvObj$minBW)
            }
        }
        else if (bwuserCovGcv == "CV") {
            gcvObj <- gcvlwls2dV2(obsGrid, regGrid, kern = kern, 
                rcov = rcov, t = t, verbose = optns$verbose, 
                CV = "10fold")
            bwCov <- gcvObj$h
        }
    }
    else if (bwuserCov != 0) {
        bwCov <- bwuserCov
    }
    if (!useBins) {
        smoothCov <- lwls2dV2(bwCov, kern, xin = rcov$tPairs, 
            yin = rcov$cxxn, xout1 = cutRegGrid, xout2 = cutRegGrid)
    }
    else {
        smoothCov <- lwls2dV2(bwCov, kern, xin = rcov$tPairs, 
            yin = rcov$meanVals, win = rcov$count, xout1 = cutRegGrid, 
            xout2 = cutRegGrid)
    }
    if (error) {
        sigma2 <- pc_covE(obsGrid, regGrid, bwCov, rotationCut = rotationCut, 
            kernel = kern, rcov = rcov)$sigma2
    }
    else {
        sigma2 <- NULL
    }
    res <- list(rawCov = rcov, smoothCov = (smoothCov + t(smoothCov))/2, 
        bwCov = bwCov, sigma2 = sigma2, outGrid = cutRegGrid)
    class(res) <- "SmoothCov"
    gc()
    return(res)
}
