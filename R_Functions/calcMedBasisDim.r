
calcMedBasisDim 	<- function(bicMat, basis_seq = seq(3,51,2)){
	optBasisDim 	<- basis_seq[median((apply(bicMat, 2, which.min)))]
	return(optBasisDim)
}
