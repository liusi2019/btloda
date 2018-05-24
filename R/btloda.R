#' This function returns both the anomaly scores and the projection results

btloda <-
function(a,sparsity=NA, maxk=1000, keep=NULL, exclude=NULL, debug=F, inf_replace = log(1e-09)) {
  a <- as.matrix(a)
  l <- nrow(a)
  d <- ncol(a)
  
  if (is.na(sparsity)) {
    sp <- ifelse(ncol(a) == 1, 0, 1-1/sqrt(ncol(a)))
  } else {
    sp <- sparsity
  }
  pvh <- bt_proj(a, maxk=maxk, sp=sp, keep=keep, exclude=exclude, debug=debug)
  
  oob_nll <- oob_get_neg_ll_all_hist(a, pvh$record_mat, pvh$pvh$w, pvh$pvh$hists, inf_replace = inf_replace)
  return(list(oob_nll = oob_nll, pvh = pvh$pvh))
}
