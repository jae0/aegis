

non_convex_hull = function( xy, alpha=NULL, plot=FALSE ) {
  #\\ using the technique from https://rpubs.com/geospacedman/alphasimple
  #\\ find the outline of a cloud of points
  #\\ see stmv::ah2sp
  
  sf_triangles = st_collection_extract( st_triangulate(do.call(c, st_geometry(xy) )  ))

  if (is.null(alpha) ) {
    lens = st_length(st_cast(sf_triangles, "LINESTRING"))
    alpha = quantile(lens, 0.99)
  }

  sf_triangles = sf_triangles[lens <= alpha]
  sfu = st_union( sf_triangles )
  sfu = st_buffer( sfu, dist=alpha/5 ) 
  return ( sfu )

  # old method .. 
  use_alpha_hull = FALSE
  if (use_alpha_hull ) {
    require(alphahull)
    require(igraph)
    o = ashape( xy[,1], xy[,2], alpha=alpha )
    u = cbind( as.character(o$edges[, "ind1"]),  as.character(o$edges[, "ind2"]))
    ograph = graph.edgelist( u, directed = FALSE)
    cutg = ograph - E(ograph)[1]
    ends = names(which(degree(cutg) == 1))
    path = get.shortest.paths(cutg, ends[1], ends[2])[[1]]
    pathX = as.numeric(V(ograph)[path[[1]]]$name)
    pathX = c(pathX, pathX[1])
    if (plot) {
      plot(o, lwd = 10, col = "gray", pch=20)
      lines(o$x[pathX, ], lwd = 2)
    }
    return( o$x[pathX, ] )
  }

}

