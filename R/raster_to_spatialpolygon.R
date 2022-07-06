raster_to_spatialpolygon = function( ras, poly, func=sum ) {
  res = crop(ras, poly)
  u = rasterize(poly, res)  #NOTE :: TODO : move to stars::st_rasterize 
  
  df=data.frame(uid=1:length(poly))
  df$value = sapply( df$uid, FUN=function(x) {i=which(values(u)==x); func(values(res)[i], na.rm=TRUE) } )
  row.names(df) = row.names(poly)

  pg = SpatialPolygonsDataFrame( poly, df )
  pg = sp::spChFIDs( pg, row.names(poly) )  #fix id's
  pg$SA = sapply(slot(pg , "polygons"), slot, "area")

  return( pg )

}
