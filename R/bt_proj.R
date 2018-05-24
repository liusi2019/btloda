

bt_proj <-
function(a, maxk=1000, sp=1-1/sqrt(ncol(a)), keep=NULL, exclude=NULL, debug=F) {
  n <- nrow(a)
  d <- ncol(a)
  
  if (debug) print(paste("sparsity", sp))

  w <- matrix(0, nrow=d, ncol=maxk)
  hists <- list()

  w <- get_random_proj(nproj=maxk, d=d, sp=sp, keep=keep, exclude=exclude)
  record_mat <- matrix(nrow = n, ncol = maxk)
  for (i in 1:maxk){
    bt_index = sample(1:n, n, replace = TRUE, prob = NULL)
    bt_data = a[bt_index, ]
    ## keep record of if one point appeared in the bootstrap sample or not
    record_mat[,i] = as.integer(c(1:n) %in% bt_index)
    hists[i] <- build_proj_hist(bt_data, matrix(w[,i], ncol = 1))
  }

  return (list(pvh=list(w=w,hists=hists), record_mat = record_mat))
}
