folded_root = function(x, p=1, inverse=FALSE, ... ) {
  # as with Box-Cox transformation as (yp−1)/p,
  # limit when p→0, the logit function logit(p)=log(p)−log(1−p).
  # reference: https://stats.stackexchange.com/questions/399916/how-to-back-transform-a-folded-root/400119
  # default p = 1 .. ie.  expand scale from (0,1) to (-1, 1)
  # if      p = 1/2 .. ie.  expand scale from (0,1) to (-2, 2), etc
  # if      p -> 0 .. ie.  expand scale from (0,1) to (-Inf, Inf),  .. ie. logit ..etc

  if (!inverse) {
    i = which(x < 0)
    if ( length(i) > 0 ) {
      x[i] = 0
      message( "folded_root:: x was less than 0; truncating to 0" )
    }
    i = which(x > 1)
    if ( length(i) > 0 ) {
      x[i] = 1
      message( "folded_root:: x was greater than 1; truncating to 1" )
    }
    (x^p - (1-x)^p)/p
  } else {
    out = NULL
    Q = x  # rename to make it less confusing
    for ( i in 1:length(Q)) {
      q = Q[i]
      out = c(out,
        stats::uniroot( function(z) { folded_root(z, p) - q} , interval=c(0, 1), extendInt="no", ...)$root
      )
    }
    return(out)
  }
}
