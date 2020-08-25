xy.to.SpatialPolygon = function( xy, id=1, project_to=NA ) {
  #\\Convert xy matrix of coordinates (lon,lat) to a spatialpolygon
  SpatialPolygons( list(
    Polygons( list( Polygon( coords=xy)), id ) ), proj4string=sp::CRS(project_to) )
}

