
plotDerivatives <- function(calcDerivatives_obj, simMeanBias_obj, der = 1, main = paste0("Bspline with first derivatives"), X_true_mean = 0){
	matplot(calcDerivatives_obj$X_der_cd , x = simMeanBias_obj$comp_dom, lty = 1, col = addAlpha("black", alpha = .5), type = "l", xlab = "t", main = main)
	for( j in 1:dim(calcDerivatives_obj$X_der_sd)[2]) { # j =1
		lines(x = simMeanBias_obj$small_dom, y = calcDerivatives_obj$X_der_sd[ ,j], col = addAlpha("blue", alpha =0.4))
	}
	
	
	lines(calcDerivatives_obj$X_der_emp_mean, x = simMeanBias_obj$comp_dom, lty = 1, col = "black", lwd = 3)
	abline(v = simMeanBias_obj$comp_dom, col = addAlpha("black", alpha = .2))
	legend("topright", legend = c("Small Domain ", "Complete Domain", "Pooled emp. Mean"),
               text.width = strwidth("1,000,000"),
               lty = c(rep(1, times = 3)), col=c(addAlpha("blue", alpha =0.4), addAlpha("black", alpha = .5), "black"),
			   lwd = 3,
               title = "Small and complete Domain")
	
	addCornerText(paste0("y=0-Mean: ", calcDerivatives_obj$y0_mean, "\ny=1-Mean: ", calcDerivatives_obj$y1_mean, "\n L2 Dist: ", calcDerivatives_obj$dist), location = "topleft")
	simDerPlot <- recordPlot()	
}
