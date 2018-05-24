pdf_hist <-
function(x, h) {
  n <- length(x)
  pd <- rep(0, n)
  for (j in 1:n) {
    i <- binsearch(h$breaks, x[j])
    # -1 adjustment since the upper index into the array is returned
    if (i > 1) i <- i - 1
    # More accurately, we should also multiply by diff(h$breaks)[i]; 
    # however, all breaks are equal in length in this algorithm,
    # hence, ignoring that for now.
    pd[j] <- h$density[i]
  }
  return (pd)
}
