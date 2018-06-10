#' This function returns the anomaly scores, the projections and histograms built from using bootstrap and out-of-bag version of loda
#' @export
#' @param a Input dataframe or data matrix
#' @param sparsity The proportion of the dimensions assigned to be 0 when build each random projection. Default is NA, in which case if the data dimension d > 1, 1 - d^{\{-1/2\}} proportion of dimensions will be set to 0, and if d = 1, no dimension will be set to 0. 
#' @param maxk The number of projections/histograms we want to build.
#' @param inf_replace The quantity to use to replace the possible negative infinity got from calculating log density based on the histograms built. Default is log(1e-09).
#' @return A list consisting of the anomaly scores, the projections and histograms built.
#' @examples
#' a = matrix(rnorm(200, 0, 1), nrow = 20, ncol = 10)
#' bt_out = btloda(a,sparsity=NA, maxk=1000, inf_replace = log(1e-09))
#' bt_out$oob_nll 
#' bt_out$pvh$w
#' bt_out$pvh$hists
btloda <-
function(a,sparsity=NA, maxk=1000, inf_replace = log(1e-09)) {
  a <- as.matrix(a)
  l <- nrow(a)
  d <- ncol(a)
  
  if (is.na(sparsity)) {
    sp <- ifelse(ncol(a) == 1, 0, 1-1/sqrt(ncol(a)))
  } else {
    sp <- sparsity
  }
  pvh <- bt_proj(a, maxk=maxk, sp=sp)
  
  oob_nll <- oob_get_neg_ll_all_hist(a, pvh$record_mat, pvh$pvh$w, pvh$pvh$hists, inf_replace = inf_replace)
  return(list(oob_nll = oob_nll, pvh = pvh$pvh))
}
