build_proj_hist <-
function(a, w) {
  d <- ncol(w)
  x <- a %*% w
  hists <- list()
  for (j in 1:d) {
    hists[[j]] <- histogram(x[,j], type="regular", plot=F, verbose=F)
  }
  return (hists)
}
