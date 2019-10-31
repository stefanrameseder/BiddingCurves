print.BCD <- function(x){
	cat(paste0("Selected Basis Length: ", x$selBasis, "\n"))
	cat(paste0("Correlation of d and 1. Score: ", round(x$cor,2), "\n"))
	if(isTRUE(x$romWolf$ent[1]) && isTRUE(!all(x$romWolf$ent[-1]))){
		app <- "applicable"
	} else{
		app <- "not applicable"
	}
	cat(paste0("FTC Estimation is ", app, " ,i.e., only depende on first score and no other.\n"))
	cat(paste0("Romano Wolf p-Value: ", x$romWolf$pval, "\n"))
}



