#' Take data, projections and histograms as input, return the anomaly scores of the data.
#' @export
#' @param a Input dataframe or data matrix.
#' @param w Projections.
#' @param hists Hisograms.
#' @param inf_replace The quantity to use to replace the possible negative infinity got from calculating log density based on the histograms built. Default is log(1e-09).
#' @return Anomaly scores.
#' @examples 
#' a = matrix(rnorm(200, 0, 1), nrow = 20, ncol = 10)
#' bt_out = btloda(a,sparsity=NA, maxk=1000, inf_replace = log(1e-09))
#' b = matrix(rnorm(500, 0, 1), nrow = 50, ncol = 10)
#' get_neg_ll_all_hist(b, bt_out$pvh$w, bt_out$pvh$hists, inf_replace = log(1e-09))
get_neg_ll_all_hist <-
function(a, w, hists, inf_replace=log(1e-09)) {
  a = as.matrix(a)
  x <- a %*% w
  k <- length(hists)
  pds <- matrix(0, nrow=nrow(x), ncol=k)
  for (j in 1:k) {
    pds[,j] <- pdf_hist(x[,j], hists[[j]])
  }
  pds <- log(pds)
  if (!is.na(inf_replace)) pds[is.infinite(pds)] = inf_replace
  ll <- -apply(pds, 1, mean) # neg. log-lik
  return (ll)
}
