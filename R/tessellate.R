
tessellate = function(xy, outformat="sf", method="sf", crs=NULL ) {
  # create a tiled geometry

  if (anyDuplicated(xy)) message("duplicates found, they should be dropped before being sent to tesselate")

  if (method=="deldir") {
    require(deldir)
    # triangulate and tessilate
    vd = deldir::deldir( xy[,1], xy[,2], z=1:nrow(xy) )
    w  = deldir::tile.list(vd)
    require(sp)
    polys = vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
      pcrds = cbind(w[[i]]$x, w[[i]]$y)
      pcrds = rbind(pcrds, pcrds[1,])
      polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i) )
    }
    spp = SpatialPolygons(polys)
    sfpoly = as( spp, "sf")
    if (!is.null(crs)) st_crs(sfpoly) = crs
    sfpoly = st_make_valid(sfpoly)
  }

  if (method=="sf") {
    require(sf)
    mp = st_multipoint(xy)
    bb =  st_as_sfc(st_bbox( mp ))

    # sfpoly = st_sfc(st_collection_extract( st_voronoi(mp, bb)  ) ) #  plot(sfpoly, col=0)
    sfpoly = st_sfc(st_collection_extract( st_voronoi(do.call(c, st_geometry(mp)), bb)  ) )  
    # st_voronoi does not keep order:   and drops duplicates well
    o = unlist(st_intersects(mp, sfpoly)) # find the unique sort of polygons to points
    sfpoly = st_make_valid(sfpoly[o])
    if (!is.null(crs)) st_crs(sfpoly) = crs
  }

  if (outformat=="sf") return( sfpoly )
  if (outformat=="sp") return( as(sfpoly, "Spatial") )

  if (0) {
        # matching Voronoi polygons to data points:
        # https://github.com/r-spatial/sf/issues/1030
        # generate 50 random unif points:
        n = 100
        pts = st_as_sf(data.frame(matrix(runif(n), , 2), id = 1:(n/2)), coords = c("X1", "X2"))
        # compute Voronoi polygons:
        pols = st_collection_extract(st_voronoi(do.call(c, st_geometry(pts))))
        # match them to points:
        pts$pols = pols[unlist(st_intersects(pts, pols))]
        plot(pts["id"], pch = 16) # ID is color
        plot(st_set_geometry(pts, "pols")["id"], xlim = c(0,1), ylim = c(0,1), reset = FALSE)
        plot(st_geometry(pts), add = TRUE)

        point <- st_sfc(lapply(replicate(100, runif(2), simplify=FALSE), st_point))
        polygon <- st_intersection(st_collection_extract(st_voronoi(do.call(c, point))), border)
        o <- unlist(st_intersects(point, polygon)) # find the unique sort of polygons to points

  }

}
