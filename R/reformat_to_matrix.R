reformat_to_matrix = function( input, matchfrom, matchto ) {
  out = matrix( NA, nrow=length(matchto[[1]]), ncol=length(matchto[[2]]), dimnames=matchto )
  rows = match( matchfrom[[1]], matchto[[1]] )
  cols = match( matchfrom[[2]], matchto[[2]] )
  out[ cbind( rows, cols ) ] = input
  return( out )
}
