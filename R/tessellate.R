
tessellate = function(xy, outformat="sf" ) {
  # create a tiled geometry
  require(deldir)
  # triangulate and tessilate
  vd = deldir::deldir( xy[,1], xy[,2], z=1:nrow(xy) )
  w  = deldir::tile.list(vd)

  if (outformat=="sp") {
    require(sp)
    polys = vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
      pcrds = cbind(w[[i]]$x, w[[i]]$y)
      pcrds = rbind(pcrds, pcrds[1,])
      polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i) )
    }
    spp = SpatialPolygons(polys)
    return(spp)
  }

  if (outformat=="sf") {
    require(sf)
    polys = vector(mode='list', length=length(w))
    for (i in seq(along=polys)) {
      pcrds = cbind(w[[i]]$x, w[[i]]$y)
      pcrds = rbind(pcrds, pcrds[1,])
      polys[[i]] = list(  as.matrix( pcrds) )
    }
    sfpoly = st_multipolygon(polys)
    return(sfpoly)
  }


}
