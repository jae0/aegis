stan_extract = function( x, vns=NULL ) {

  # extract and reformat a variable from a cmdstanr "draw" object , similar to rstan::extract in function 
  # x = as_draws_df( fit$draws() )
  # u = stan_extract( x, "B" )
  require(term)
  
  out = list()
  
  names_x = setdiff(  names(x) , c(".chain", ".iteration", ".draw", "lp__") )
  niter = dim(x)[1]

  if (is.null(vns)) vns = term::pars( term::term( names_x ) )
  
  for (vn in vns) {
    cc = grep( paste("^", vn, "\\[.*", sep=""), names_x ) 
    yy = names_x[cc] 
    dd = term::pdims( term::term( yy ) )
    out[[vn]] = array( as.matrix( x[,cc] ), dim=c(niter, dd[[vn]]) )
  }

  return(out)

}


