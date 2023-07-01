

non_convex_hull = function( xy, plot=FALSE, lengthscale=NULL, method="voronoi" ) {

  # best that xy be planar (km)
  bb = sf::st_bbox( xy)
  xr = bb["xmax"] - bb["xmin"]
  yr = bb["ymax"] - bb["ymin"]


  if (method=="alpha_hull" ) {
      # old method .. 
      #\\ using the technique from https://rpubs.com/geospacedman/alphasimple
      #\\ find the outline of a cloud of points
      #\\ see stmv::ah2sp
      require(alphahull)
      require(igraph)

      if( is.null(lengthscale) ) lengthscale = signif( min(c(xr, yr)) / 100, 2 ) 
      
      xy = st_coordinates(xydata)
      xy = unique(xy)

      o = ashape( xy[,1], xy[,2], alpha=lengthscale )
      u = cbind( as.character(o$edges[, "ind1"]),  as.character(o$edges[, "ind2"]))
      ograph = graph.edgelist( u, directed = FALSE)
      cutg = ograph - E(ograph)[1]
      ends = names(which(degree(cutg) == 1))
      path = get.shortest.paths(cutg, ends[1], ends[2])[[1]]
      pathX = as.numeric(V(ograph)[path[[1]]]$name)
      pathX = c(pathX, pathX[1])
      if (0) {
        plot(o, lwd = 10, col = "gray", pch=20)
        lines(o$x[pathX, ], lwd = 2)
      }
      out = as.data.frame(o$x[pathX,])
      colnames(out) = c("X", "Y")
      out = sf::st_as_sf( out, coords=c("X", "Y")  )
      return( out )
  }

  if (method=="voronoi" ) {
      
    if( is.null(lengthscale) ) lengthscale = signif( min(c(xr, yr)) / 1000, 2 ) 

    xy$uid = 1:nrow(xy)
    xy_raster = stars::st_rasterize( xy["uid"], dx=lengthscale, dy=lengthscale )
    xy_pts = sf::st_as_sf( xy_raster, as_points=TRUE, na.rm=FALSE )
    xy_pts = xy_pts[ which(is.finite(xy_pts$uid)), ]
    st_crs(xy_pts) = st_crs( xy ) 
  browser()
    sf_triangles = st_collection_extract( st_triangulate(do.call(c, st_geometry(xy_pts) )  ))
    st_crs(sf_triangles) = st_crs( xy ) 

    lens = st_length(st_cast(sf_triangles, "LINESTRING"))
    units(lens) = NULL
    lenll = max( quantile(lens, 0.95), lengthscale*10 )
    
    sfu = sf_triangles[ which(lens <= lenll) ]
    sfu = st_union( sfu )
    sfu = st_buffer( sfu, dist=lengthscale ) 
    
    return ( sfu )
  }
  
}

