#' get negative loglikelihood from all histograms
get_neg_ll_all_hist <-
function(a, w, hists, inf_replace=NA) {
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
