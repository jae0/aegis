nb_remove = function( NB_graph, u, v=NULL ) {

  if ( !is.numeric(u)) u = which(attr(NB_graph, "region.id") %in% u)

  if (is.null(v)) {
    # remove all instances of a reference to u
    u = sort(u, decreasing=TRUE)
    NB_graph[u] = 0L # flag in case
    for (v in u) {
      for (i in 1:length(NB_graph)){
        a = which(NB_graph[[i]] == v)
        if (length(a) > 0)  NB_graph[[i]] = NB_graph[[i]][-a]  # remove self
        o = which(NB_graph[[i]] > v)
        if (length(o) > 0)  NB_graph[[i]][o] = NB_graph[[i]][o] - 1L # shift all references to values greateer than self
      }
    }
    NB_graph[u] = NULL
    attr(NB_graph, "region.id") = attr(NB_graph, "region.id")[-u]

  } else {

    if (length(u) != length(v) ) stop ("u and v must be of the same length")
    for( i in 1:length(u)) {
      NB_graph[[u[i]]] = as.integer( sort( unique( setdiff( NB_graph[[u[i]]], v[i] ) ) ) )
      NB_graph[[v[i]]] = as.integer( sort( unique( setdiff( NB_graph[[v[i]]], u[i] ) ) ) )
    }
  }

  # deletion can create zero neighbours .. remove empty sets
  w = NULL
  for ( i in 1:length(NB_graph) ) if (length(NB_graph[[i]]) == 0) w = c(w, i)
  if (length(w) >0)  NB_graph = nb_remove( NB_graph, w )

  return(NB_graph)
}
