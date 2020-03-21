

non_convex_hull = function( xy, alpha, plot=FALSE ) {
  #\\ using the technique from https://rpubs.com/geospacedman/alphasimple
  #\\ find the outline of a cloud of points
  #\\ see stmv::ah2sp
  require(alphahull)
  require(igraph)
  o = ashape( xy, alpha=alpha )
  ograph = graph.edgelist( cbind( as.character(o$edges[, "ind1"]), 
                                  as.character(o$edges[, "ind2"])), directed = FALSE)
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

