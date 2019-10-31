## 
getMeanDense <- function (yu_m, optns) {
# Input:
# - pooled Y- U - Matrix
# - optns: List of chosen options
# Output:
# - cross sectional means only for dense data or dense data with missing values
    if (!(optns$dataType %in% c("Dense", "DenseWithMV"))) {
        stop("Cross sectional mean is only applicable for option: dataType = \"Dense\" or \"DenseWithMV\"!")
    }
    mu = colMeans(yu_m, na.rm = TRUE)
    ret = list(mu = mu, muDense = NULL, mu_bw = NULL)
    class(ret) = "SMC"
    if (any(is.na(mu))) {
        stop("The cross sectional mean is appears to have NaN! Consider setting your dataType to 'Sparse' manually")
    }
    gc()
    return(ret)
}