discretize_data = function( x, brks ) {
  # discretize data x along the break points .. and midpointss as labels
  return( as.numeric( as.character( cut( x, breaks=brks, labels=diff(brks)/2 + brks[-length(brks)], include.lowest=TRUE ) )))
}
