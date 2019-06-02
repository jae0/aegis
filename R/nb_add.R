nb_add = function( nb, u, v ) {
  if (length(u) != length(v) ) stop ("u and v must be of the same length")

  w = unique(c(u,v))
  if ( any( w > length(nb) ) ) {
    # new locations are being added
     x = w[ which( w > length(nb) ) ]
     attr(nb, "region.id") = c( attr(nb, "region.id"), x )  # extend
  }
  for( i in 1:length(u)) {
    nb[[u[i]]] = as.integer( sort( unique( c( setdiff(nb[[u[i]]],0), v[i] ) ) ))
    nb[[v[i]]] = as.integer( sort( unique( c( setdiff(nb[[v[i]]],0), u[i] ) ) ))
  }
  return(nb)
}
