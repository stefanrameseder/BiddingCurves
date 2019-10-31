									
est_FTC_Mean_given_ind <- function(ind, pmhtnt){ # ind <- T-B
	# Explanation:
	# estimate FTC Mean given sample up to index ind 
	
	# define training sample 
	trainSample <- evalBidcurves[[pmhtnt]][1:ind]
	
	# predict mean given training sample
	train_fcts 	<- sapply(trainSample, function(x) x[[1]])
	train_ders 	<- sapply(trainSample, function(x) x[[2]])

	est_ftc 	<- calcFTC(	fds = train_fcts, 
							comp_dom = md_dis,
							basis_seq = seq(3,maxBasisLength,2), 
							base = basis_choice,
							maxBasisLength = maxBasisLength, 
							basisChoice = basisSel,
							alpha = 0.05, B = B, 
							derFds = train_ders)
	
	
	
	
	# matplot(x = md_dis, y=sapply(evalBidcurves[[pmhtnt]], function(x) x[[1]]), # dim(combinedNEG)
        # col = PartiallyFD:::addAlpha("black", 0.2), type = "l", lwd = 1,
        # ylab = "", xaxt = "n", yaxt = "n", lty = "solid")
	# lines(x= md_dis, y = est_ftc$ ftcMean, col = "red", lwd = 2)
	
	return(est_ftc)
}

