nb_add = function( NB_graph, u, v ) {
  if (length(u) != length(v) ) {
    warning ("u and v must be of the same length")
    return(NB_graph)
  }

  w = unique(c(u,v))
  if ( any( w > length(NB_graph) ) ) {
    # new locations are being added
     x = w[ which( w > length(NB_graph) ) ]
     attr(NB_graph, "region.id") = c( attr(NB_graph, "region.id"), x )  # extend
  }
  for( i in 1:length(u)) {
    NB_graph[[u[i]]] = as.integer( sort( unique( c( setdiff(NB_graph[[u[i]]],0), v[i] ) ) ))
    NB_graph[[v[i]]] = as.integer( sort( unique( c( setdiff(NB_graph[[v[i]]],0), u[i] ) ) ))
  }
  return(NB_graph)
}
