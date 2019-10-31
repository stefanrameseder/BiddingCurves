

plotSimMeanBias<- function(simMeanBias_obj, main = paste0("Bspline with ", nbasis , ", ", n ," Functions")){
	#dev.off()
	ylim					<- c(-20,20)
	boxcolor_ci				<- "darkgrey" # rgb(190, 190, 190, alpha=80, maxColorValue=255)
	boxcolor_max			<- "lightgrey" # rgb(80, 80, 80, alpha=80, maxColorValue=255)
	scl     				<- 1.2
	scl.axs 				<- 1.2
	scl.lab 				<- 1.2
	par(mar=c(5.1, 5.1, 4.1, 2), mfrow=c(1,1), cex.lab=1.4, cex.axis=1.4, cex=1.4)
	matplot(simMeanBias_obj$X_cd_m , x = simMeanBias_obj$comp_dom, lty = 1, 
			col = addAlpha("black", alpha = .3), type = "l", ylab = expression(paste(X[t])), xlab = expression(tau), main = main, ylim = ylim)
		for( j in 1:dim(simMeanBias_obj$X_sd_m)[2]) { # j =1
			lines(x = simMeanBias_obj$small_dom, y = simMeanBias_obj$X_sd_m[ ,j], col = addAlpha("blue", alpha =0.4))
		}
	rng						<- range(simMeanBias_obj$comp_dom)
	rect(rng[1], ylim[1], rng[2], ylim[2], border = "lightgrey", col = boxcolor_max)
	rng						<- range(simMeanBias_obj$small_dom)
	rect(rng[1], ylim[1], rng[2], ylim[2], border = "darkgrey", col = boxcolor_ci)
	
	matplot(simMeanBias_obj$X_cd_m , x = simMeanBias_obj$comp_dom, lty = 1, 
			col = addAlpha("black", alpha = .3), type = "l", ylab = expression(paste(X[t])), xlab = "t", main = main, ylim = ylim, add = TRUE)
		for( j in 1:dim(simMeanBias_obj$X_sd_m)[2]) { # j =1
			lines(x = simMeanBias_obj$small_dom, y = simMeanBias_obj$X_sd_m[ ,j], col =addAlpha("black", alpha = .3))
		}
	
	 
	lines(simMeanBias_obj$X_emp_mean, x = simMeanBias_obj$comp_dom, lty = 1, col = "darkgreen", lwd = 3)
	abline(h = 0, col =addAlpha("red", alpha = .7), lwd =3, lty = 2)
	abline(v = 1, col =addAlpha("black", alpha = .5), lwd = 3, lty = 1)
	abline(v = rng[2], col = "blue")
	legend(	"topleft", legend = c("Small Domain ", "Complete Domain", "Pooled emp. Mean", "True Mean", "FTC Mean"), inset = 0.01, bg = "white",
               #text.width = strwidth("1,000,000"),
               lty = c(rep(1, times = 3), 2,1), col=c(addAlpha("blue", alpha =0.4), addAlpha("black", alpha = .5), "darkgreen", "red", "lightgreen"),
			   lwd = 3)
	#dev.off()
	#addCornerText(paste0("y=0-Mean: ", simMeanBias_obj$y0_mean, "\ny=1-Mean: ", simMeanBias_obj$y1_mean, "\n L2 Dist: ", simMeanBias_obj$dist), location = "topleft")
}

