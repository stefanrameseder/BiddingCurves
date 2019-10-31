


detectOutlierFD <- function( curvedata, com_grid , depthtype = "FMD" , trace=FALSE ){
    ## INPUT:
    ## - curvedata: N x T matrix of evaluated curves at com_grid
    ## - com_grid:  vector of length T, common grid for evaluation
    ## - depthtype: string, either MD or FMD

    ## OUTPUT:
    ## - OutIndex: index of curves, classified as outliers
    N  <- dim(curvedata)[1]
    T  <- dim(curvedata)[2]

    curCurves   <- curvedata
    InIndex     <- 1:N
    OutIndex    <- NULL
    while (TRUE){
        ## compute C for curCurves
        threshold <- fdaBootstrap(curCurves , com_grid , computeC = TRUE)

        ## compute depth (step 1)
        if (depthtype=="MD"){
            depths <- modalDepth(curCurves, com_grid)
        } else {
            depths <- fmDepth(curCurves, com_grid)
        }
        outliers  <- depths <= threshold
        if ( sum(outliers) == 0 ) break
        OutIndex  <- c( OutIndex, InIndex[outliers])
        if (trace) cat( paste("detected ", as.character(InIndex[outliers]), " as outlier\n") )
        InIndex   <- InIndex[!outliers]  # new one
        curCurves <- curCurves[!outliers,] # new one
    }
    return(OutIndex)
}
