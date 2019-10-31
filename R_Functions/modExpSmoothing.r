
modExpSmoothing <- function(y, H, threshold = 0.02){ # y <- data[[1]][[1]][,6]; H =1; plot(c(y, fc), type = "l"); points(length(y), fc, col = "red", pch =16)  
	if( sum( y > 0.01) > 8){
		model 	<- ets(	y = y, model="ZZZ", damped=NULL, alpha=NULL, beta=NULL, gamma=NULL,
						phi=1, additive.only=FALSE, lambda=NULL,
						lower=c(rep(0.0001,3), 0.8), upper=c(rep(0.9999,3),1),
						opt.crit="mae", nmse=1,bounds="usual", ic="bic", restrict=FALSE, 
						use.initial.values=FALSE)
						
		if(any(( fc  <- round(forecast(model, h = H)$mean, 2)) < threshold)){ # if fc is smaller than 20 cent
		  fc[fc< threshold] <- 0
		}
		return(list(model = model, forecast =  list(pred = fc, se = forecast(model, h = H)$mean)))
	} else { # just return 0s
		return(list(model = 0, forecast =  list(pred = 0, se = 0)))
	}
}
