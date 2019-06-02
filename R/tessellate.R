
tessellate = function(xy) {
  # create a tiled geometry 
  require(sp)
  # triangulate and tessilate
  require(deldir)
  vd = deldir::deldir( xy[,1], xy[,2], z=1:nrow(xy) )
  w  = deldir::tile.list(vd)
  polys = vector(mode='list', length=length(w))
  for (i in seq(along=polys)) {
    pcrds = cbind(w[[i]]$x, w[[i]]$y)
    pcrds = rbind(pcrds, pcrds[1,])
    polys[[i]] = Polygons(list(Polygon(pcrds)), ID=as.character(i) )
  }
  SP = SpatialPolygons(polys)
  return(SP)
}
