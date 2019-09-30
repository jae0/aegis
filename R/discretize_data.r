discretize_data = function( x, brks, digits=NULL ) {
  # discretize data x along the break points .. and midpointss as labels
  o = as.numeric( as.character( cut( x, breaks=brks, labels=diff(brks)/2 + brks[-length(brks)], include.lowest=TRUE ) ))
  if (!is.null(digits)) o = signif(o, digits=digits )
  return( o )
}
