binsearch <-
function(arr, x) {
  # Assumes arr contains the histogram bin breaks.
  # Note: Returns the index of the upper boundary of interval
  # since we assume the intervals are right closed i.e. (l,r]
  l <- 1
  r <- length(arr)
  if (x <= arr[l]) return (l)
  if (x >= arr[r]) return (r)
  while (l < r) {
    p <- (l+r) %/% 2
    if (x > arr[p]) {
      l <- p
    } else if (x <= arr[p]) {
      r <- p
    }
    if (l == r-1 && x <= arr[r]) l <- r
  }
  return (r)
}
