interval_break = function(X, n, style="quantile", extend=0.01) {
  # wrap around classIntervals and make termimnal bounds a bit larger
  i = which( is.finite(X) )
  if (length(i) > 0 ) X = X[i]
  BK = classInt::classIntervals( X, n=n, style=style)$brks
  ext = abs( extend * BK[c(1,length(BK))] )
  BK[1] = BK[1] - ext[1]
  BK[length(BK)] = BK[length(BK)] + ext[2]
  return(BK)
}
