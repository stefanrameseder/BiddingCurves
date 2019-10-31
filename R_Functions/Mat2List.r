
#############################################
## Mat2List
Mat2List <- function (y_m, d=2, na.rm = FALSE) {
	# Converts a Matrix y_m into a list corresponding to columns = 2 or rows = 1
    # Input:
	#- y_m a Matrix 
	#- d a number 1 or 2 corresponding to rows or columns
	# Output:
	#- list consisting of rows or colums of the matrix y_mat
	if(!is.matrix(y_m) || !(d %in% c(1,2) )){
		stop("y_m is not a Matrix or d is not correctly specified!")
	}
	if(d == 1){ # row case
		if(na.rm == FALSE){
			y_l <- lapply(seq_len(nrow(y_m)) , function(i) y_m[i, ])
		} else{
			y_l <- lapply(seq_len(nrow(y_m)) , function(i) na.omit(y_m[i, ]))
		}
	} else if( d==2 ){
		if(na.rm == FALSE){
			y_l <- lapply(seq_len(ncol(y_m)) , function(i) y_m[ ,i])
		} else{
			y_l <- lapply(seq_len(ncol(y_m)) , function(i) na.omit(y_m[ ,i]))
		}
	}
    return(y_l)
}
