
presence.absence = function( X, px=NULL, include_zero_values_in_quantiles=FALSE ) {

  if( is.null(px)) stop("Must define px, the lower probability to consider no observation of significance in the quanttile distribution")
  # calc quantiles
  
  nX = length(X)
  probs = pa = rep( NA, nX )

  zerovalues = which( X == 0 ) # zero valued
  
  if (include_zero_values_in_quantiles) {
    si = 1:nX    # all values
  } else {
    si = which( X > 0)  # positive values
  }

  # operating upon empirical qunatiles
  Y = X[si]
  i = which( is.finite( Y ) )
  pr = ecdf(Y[i]) ( Y[i] )
  ix = which( pr ==1 )
  if ( !{ length(ix) %in% c(0, nX) } )  pr[ix] = max( pr[-ix] )
  Y[i] = pr

  probs[si] = Y

  sb = which( probs < px )  # buffer zone
  s0 = unique( c(sb, zerovalues ) )
  s1 = which( probs >= px )

  # presence-absence
  pa[s1] = 1
  pa[zerovalues] = 0
  pa[s0] = 0

  # after the above is set, modify for those with values known to be zero
  probs[zerovalues] = 1

  # determine appropriate weights
  # quantiles less than px have a weight that is 1-px  -- maximum close to zero
  probs[s1] = pmax( 0.01, probs[s1] )
  if (length(sb)>0) probs[sb] = pmax( 0.01,  1 - probs[sb] )  # inverting probs
  probs[zerovalues] = 1

  return ( list(data=X, pa=pa, probs=probs ) )  # returns/adds pa and X$wt to data frame which represents the P/A and relative confidence of correctness
}
