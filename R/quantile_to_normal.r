
quantile_to_normal = function( x, mean =0.5, ci=0.999 ) {
  # zscore transform but constrained to have mean 0.5 and ci% prob range between 0,1
  #' convert quantile to normal with mean 0.5 and variance such that ci% are found within the range (0,1)

  xmin = min( x[ which(x>0) ], na.rm=T )
  xmax = max( x[ which(x<1) ], na.rm=T )

  pr = {1 - ci} / 2  # two-tailed prob in normal approximation desired
  zsd = 2 * abs( qnorm( pr, mean=0, sd=1) )  # range in x required to have ci% of the population, two tailed
  rsd = 1 / zsd # sd required for "normal" population to fit in (0,1)

  zscore = qnorm( x, mean=mean, sd=rsd )

  xx = which( x>=1 | zscore > 1)
  if (length(xx) > 0 ) zscore[ xx ] = 1

  nn = which( x<=0 | zscore < 0)
  if (length(nn) > 0 ) zscore[ nn ] = 0

  attr(zscore, "zsd") = zsd
  attr(zscore, "rsd") = rsd
  attr(zscore, "rmean") = mean
  attr(zscore, "ci") = ci

  return (zscore)
}
