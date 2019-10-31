

mixingDep 		<- Vectorize(function(x = 0, cat = 0, sim_data){ # df = sim_data, cat = 0
	# Calculates the mixing dependency
	# f(x, cat = P(X <= x; Y = c) - P(X<=x)P(Y=c)
	
	if(any(c(colnames(sim_data)[1] != "x", colnames(sim_data)[2] != "d"))) stop("Column names do not fit!")

	return(y= sum(sim_data[ , 1] < x & sim_data[ , 2] == cat)/dim(sim_data)[1] - ( sum(sim_data[ , 1] < x)/dim(sim_data)[1] * sum(sim_data[ , 2] == cat)/dim(sim_data)[1]))
}, "x")