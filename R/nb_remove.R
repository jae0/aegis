nb_remove = function( nb, u, v=NULL ) {

  if ( !is.numeric(u)) u = which(attr(nb, "region.id") %in% u)

  if (is.null(v)) {
    # remove all instances of a reference to u
    u = sort(u, decreasing=TRUE)
    nb[u] = 0L # flag in case
    for (v in u) {
      for (i in 1:length(nb)){
        a = which(nb[[i]] == v)
        if (length(a) > 0)  nb[[i]] = nb[[i]][-a]  # remove self
        o = which(nb[[i]] > v)
        if (length(o) > 0)  nb[[i]][o] = nb[[i]][o] - 1L # shift all references to values greateer than self
      }
    }
    nb[u] = NULL
    attr(nb, "region.id") = attr(nb, "region.id")[-u]

  } else {

    if (length(u) != length(v) ) stop ("u and v must be of the same length")
    for( i in 1:length(u)) {
      nb[[u[i]]] = as.integer( sort( unique( setdiff( nb[[u[i]]], v[i] ) ) ) )
      nb[[v[i]]] = as.integer( sort( unique( setdiff( nb[[v[i]]], u[i] ) ) ) )
    }
  }

  # deletion can create zero neighbours .. remove empty sets
  w = NULL
  for ( i in 1:length(nb) ) if (length(nb[[i]]) == 0) w = c(w, i)
  if (length(w) >0)  nb = nb_remove( nb, w )

  return(nb)
}
