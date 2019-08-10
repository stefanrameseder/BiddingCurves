# BiddingCurves
This repository contains an R codes for the Paper "Bidding Curve Dynamics" by Stefan Rameseder. 

The underlying statistical method is described in:  
    [toDo](https://arxiv.org/abs/1711.07715)  
by Stefan Rameseder (arXiv:1711.07715)

In addition, we provide .r files to reproduce the full simulation study.
- PartiallyFD_simulation.r: Given all meta parameters, simulate the DGPs and apply estimators
- PartiallyFD_simulation_analysis.r: Calculate the KPIs (and many more) used in the paper

## Installation of required packages
```r
install.packages("devtools")
install.packages("fda")
install.packages("mclust")
library("mclust")
library("devtools")
```

