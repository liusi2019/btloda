
get_random_proj <-
function(nproj, d, sp, keep=NULL, exclude=NULL) {
  nzeros <- (d*sp) %/% 1
  idxs <- 1:d # set of dims that will be sampled to be set to zero
  marked <- c()
  if (!is.null(keep)) marked <- c(marked, keep)
  if (!is.null(exclude)) {
    # since 'exclude' contains the dims that are
    # predetermined to be zero, adjust the number
    # of zero dims that need to be further determined
    # by sampling
    nzeros <- nzeros - length(exclude)
    marked <- c(marked, exclude)
  }
  if (length(marked) > 0) {
    # remove from the known set -- the dims that have been 
    # marked for keeping or excluding. There is no uncertainty in
    # the selection/rejection of marked dims.
    idxs <- idxs[-marked]
  }
  w <- matrix(0, nrow=d, ncol=nproj)
  for (i in 1:nproj) {
    w[,i] <- rnorm(d, mean=0, sd=1)
    if (nzeros > 0) {
      z <- sample(idxs, nzeros, replace=F)
      if (!is.null(exclude)) z <- c(z,exclude)
      w[z,i] <- 0
    }
    w[, i] <- w[, i] / sqrt(sum(w[, i] * w[, i]))
  }
  return (w)
}
