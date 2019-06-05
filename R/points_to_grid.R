
points_to_grid = function( SPDF, res=100 ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data 
  
  require(sp)
  require(rgeos)
  require(raster)
  require(fasterize)

  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    spbuffer = NULL
    proj4string=NULL
    SPDF=meuse
    res = points_to_polygons( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
  }

  raster_template = raster(extent(SPDF)) # +1 to increase the area
  res(raster_template) = res  # in meters
  crs(raster_template) = projection(SPDF) # transfer the coordinate system to the raster

  rast <- rasterize(SPDF, raster_template, names(SPDF), fun=mean, na.rm=TRUE) # not meaningful fir factors
  
  O = as(rast, "SpatialGridDataFrame")

  return(O)
}
