stan_extract = function( x, vns ) {

  # extract and reformat a variable from a cmdstanr "draw" object , similar to rstan::extract in function 
  # x = as_draws_df(fit$draws() )
  # u = stan_extract( x, "B" )

  names_x = names(x) 
  niter = dim(x)[1]

  out = list()
  for (vn in vns) {
    cc = grep( paste("^", vn, "\\[.*", sep=""), names_x ) 
    yy = names_x[cc] 
    dd = term::pdims( term::term( yy ) )
    out[[vn]] = array( as.matrix( x[,cc] ), dim=c(niter, dd[[vn]]) )
  }

  return(out)

}


