
auctionLossToRho <- function(AL){ 
	# To do a more stable optimization, we transform the Auction Loss value into a convex combination,
	# i.e., instead of Ind * bla + (1-Ind)* AL * bla, we define rho = 1/AL and then
	# calculate rho * bla + (1-rho) * bla or equivalently, p *f(x) + (1-p) g(x)
	# If AL = 19, then that means that the auction loss is 19 times higher than the other guy... or equivalently: 0.05 : 0.95
	return(rho = 1/(AL+1)) 
}
