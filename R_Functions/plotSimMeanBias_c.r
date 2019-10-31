


plotSimMeanBias_c <- function(simMeanBias_c_obj, main = paste0("Bspline with ", nbasis , ", ", n ," Functions")){

	ylim					<- c(-20,20)
	boxcolor_ci				<- "darkgrey" # rgb(190, 190, 190, alpha=80, maxColorValue=255)
	boxcolor_max			<- "lightgrey" # rgb(80, 80, 80, alpha=80, maxColorValue=255)
	scl     				<- 1.2
	scl.axs 				<- 1.2
	scl.lab 				<- 1.2
	par(mar=c(5.1, 5.1, 4.1, 2), mfrow=c(1,1), cex.lab=1.4, cex.axis=1.4, cex=1.4)

	
	plot(	simMeanBias_c_obj$X_ad_m[, 1] , x = simMeanBias_c_obj$comp_dom, lty = 1, 
			ylab = expression(paste(X[t])), xlab = expression(tau),
			ylim = ylim, col = addAlpha("black", alpha = .5), type = "l", main = main)
	rect(0, ylim[1], 1, ylim[2], border = "darkgrey", col = boxcolor_ci)	
	rect(0.5, ylim[1], 1, ylim[2], border = "lightgrey", col = boxcolor_max)
	
	for( j in 1:dim(simMeanBias_c_obj$X_ad_m)[2]) { # j =1
		lines(x = simMeanBias_c_obj$comp_dom, y = simMeanBias_c_obj$X_ad_m[ ,j], col = addAlpha("black", alpha =0.3))
	}
	
	lines(simMeanBias_c_obj$X_emp_mean, x = simMeanBias_c_obj$comp_dom, lty = 1, col = "darkgreen", lwd = 3)
	abline(h = 0, col =addAlpha("red", alpha = .7), lwd =3, lty = 2)
	abline(v = 1, col =addAlpha("black", alpha = .5), lwd = 3, lty = 1)
	abline(v = 0.5, col = "blue")
	abline(v = simMeanBias_c_obj$comp_c, col =addAlpha("blue", alpha = .2))
	legend(	"topleft", legend = c("Small Domain ", "Complete Domain", "Pooled emp. Mean", "True Mean", "FTC Mean"), inset = 0.01, bg = "white",
               #text.width = strwidth("1,000,000"),
               lty = c(rep(1, times = 3), 2,1), col=c(addAlpha("blue", alpha =0.4), addAlpha("black", alpha = .5), "darkgreen", "red", "lightgreen"),
			   lwd = 3)
}

