discretize_data = function( x, brks, labels=NULL, digits=NULL, resolution=NULL ) {
  # discretize data x along the break points .. and midpoints as labels, with signif digits digits
  
  # truncate data to break limits .. extreme being one increment lower/higher
  oo = min( abs(diff(brks)) ) * 0.1
  ii = which( x <= min(brks) ) 
  if (length(ii) > 0)  x[ii] = min(brks) + oo
  jj = which( x >= max(brks) ) 
  if (length(jj) > 0)  x[jj] = max(brks) - oo
 
  if (is.null(labels)) {
    labels = diff(brks)/2 + brks[-length(brks)]
    if (!is.null(resolution)) labels = trunc(labels / resolution)*resolution
    if (!is.null(digits)) labels = signif(labels, digits=digits )
  }

  o = as.numeric( as.character( cut( x, breaks=brks, labels=labels, include.lowest=TRUE ) ))
  
  if (!is.null(resolution)) o = trunc(o / resolution)*resolution

  if (!is.null(digits)) o = signif(o, digits=digits )
  
  return( o )
}
