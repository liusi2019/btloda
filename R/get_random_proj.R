
get_random_proj <-
function(nproj, d, sp) {
  nzeros <- (d*sp) %/% 1
  idxs <- 1:d # set of dims that will be sampled to be set to zero
  w <- matrix(0, nrow=d, ncol=nproj)
  for (i in 1:nproj) {
    w[,i] <- rnorm(d, mean=0, sd=1)
    if (nzeros > 0) {
      z <- sample(idxs, nzeros, replace=F)
      w[z,i] <- 0
    }
    w[, i] <- w[, i] / sqrt(sum(w[, i] * w[, i]))
  }
  return (w)
}
