get_neg_ll <-
function(a, w, hist, inf_replace=NA) {
  x <- a %*% w
  pdfs <- matrix(0, nrow=nrow(x), ncol=1)
  pdfs[,1] <- pdf_hist(x, hist)
  pdfs[,1] <- log(pdfs)
  if (!is.na(inf_replace)) pdfs[,1] <- apply(pdfs, 1:2, max, inf_replace)
  return(-pdfs) # neg. log-lik of pdf
}
