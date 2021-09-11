discretize_data = function( x, brks, digits=NULL ) {
  # discretize data x along the break points .. and midpointss as labels
  oo = min( abs(diff(brks)) ) * 0.1
  ii = which( x <= min(brks) ) 
  if (length(ii) > 0)  x[ii] = min(brks) + oo
  jj = which( x >= max(brks) ) 
  if (length(jj) > 0)  x[jj] = max(brks) - oo

  o = as.numeric( as.character( cut( x, breaks=brks, labels=diff(brks)/2 + brks[-length(brks)], include.lowest=TRUE ) ))
  if (!is.null(digits)) o = signif(o, digits=digits )
  return( o )
}
