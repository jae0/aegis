
reformat_to_array = function( input, matchfrom, matchto ) {

  ndims = length(matchto)

  if (ndims == 1) {
    out = array( NA, dim=length(matchto[[1]]), dimnames=matchto  )
    d1 = match( matchfrom[[1]], matchto[[1]] )
    out[ d1 ] = input

    # out = matrix( NA, nrow=length(matchto[[1]]), ncol=1, dimnames=matchto  )
    # d1 = match( matchfrom[[1]], matchto[[1]] )
    # out[ d1, 1 ] = input
  }

  if (ndims == 2) {
    out = array( NA, dim=c(length(matchto[[1]]), length(matchto[[2]])), dimnames=matchto )
    d1 = match( matchfrom[[1]], matchto[[1]] )
    d2 = match( matchfrom[[2]], matchto[[2]] )
    out[ cbind( d1, d2 ) ] = input
  }
  if (ndims == 3) {
    out = array( NA, dim=c(length(matchto[[1]]), length(matchto[[2]]), length(matchto[[3]]) ), dimnames=matchto )
    d1 = match( matchfrom[[1]], matchto[[1]] )
    d2 = match( matchfrom[[2]], matchto[[2]] )
    d3 = match( matchfrom[[3]], matchto[[3]] )
    out[ cbind( d1, d2, d3 ) ] = input
  }
  if (ndims == 4) {
    out = array( NA, dim=c(length(matchto[[1]]), length(matchto[[2]]), length(matchto[[3]]), length(matchto[[4]]) ), dimnames=matchto )
    d1 = match( matchfrom[[1]], matchto[[1]] )
    d2 = match( matchfrom[[2]], matchto[[2]] )
    d3 = match( matchfrom[[3]], matchto[[3]] )
    d4 = match( matchfrom[[4]], matchto[[4]] )
    out[ cbind( d1, d2, d3, d4 ) ] = input
  }
  if (ndims > 4) {
    stop( "you are going to have to add more here ...")
  }

  return( out )
}
