
######################################################################
## General functions for evaluate all estimated functions/derivatives at grid
evalCurves <- function(bid_curves_l, grid = com_dom_grid_v, deriv = 0){
  if(all(c(range(grid)[1] < com_dom_grid_v[1], range(grid)[2] > com_dom_grid_v[2]))){
    warning("Chosen grid does not fit into common grid!")
  }
  return( lapply(bid_curves_l, function(x) x$constrained.curve(x = grid, deriv = deriv)) )
}



