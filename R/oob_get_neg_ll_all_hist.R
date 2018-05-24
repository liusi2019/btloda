#' get out-of-bag negative loglikelihood from all histograms that didn't use the point

oob_get_neg_ll_all_hist <-
  function(a, record_mat, w, hists, inf_replace=NA) {
    use_mat =  1 - record_mat
    x <- a %*% w
    k <- length(hists)
    pds <- matrix(0, nrow=nrow(x), ncol=k)
    for (j in 1:k) {
      pds[,j] <- pdf_hist(x[,j], hists[[j]])
    }
    pds <- log(pds)
    if (!is.na(inf_replace)) pds[is.infinite(pds)] = inf_replace
    masked_pds = use_mat * pds
    oob_ll <- -apply(masked_pds, 1, sum)/apply(use_mat, 1, sum) # neg. log-lik
    return (oob_ll)
  }