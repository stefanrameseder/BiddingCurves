
fmDepth <- function( curvedata, com_grid) {

    ## date: 30.03.2016

    ##### Frairman/Muniz Depth, according to Febrero-Bande (2007)
    ##### 'Outlier detection in functional data...'
    
    ### INPUT:
    ## - curvedata: a matrix with N functional observations, each row per observation
    ## - com_grid: T argument values, where functions are evaluated

    ### OUTPUT:
    ## - SFMD: vector of length N where each entry is the i'th curve's depth
    
    N  <- dim(curvedata)[1]
    T  <- dim(curvedata)[2]
    
    empDist <- function( index, curvedata){
        sums   <- rowSums( t( curvedata) <= curvedata[index,])
        dist   <- sums/N
        return(dist)
    }

    ## T x N matrix where a_ij is the j'th curve's cumDist at eval point x_i
    empDistr_m <- matrix( unlist( lapply( 1:N, empDist, curvedata=curvedata)), nrow=T , ncol=N)
    
    ##empDistr_m[,1]==lapply( 1:N, empDist, curvedata=curvedata)[[1]]
    
    deltas <- com_grid[2:T] - com_grid[1:T-1]
    SFMD   <- colSums( ( 1 - abs( 0.5 - empDistr_m[2:T,])) * deltas)
    return(SFMD)
}
