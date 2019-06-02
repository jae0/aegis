
aegis_db_spatial_object = function( spatial.domain="SSE", proj4string="+proj=utm +ellps=WGS84 +zone=20 +units=km", resolution=1, returntype="SpatialPixelsDataFrame" ) {
  require(sp)
  require(raster)
  # return base layout of aegis grids as a Spatial* object
  # bathymetry is the basis of all grids
  plocal = list(spatial.domain=spatial.domain)
  bathymetry = bathymetry.db(p=plocal, DS="baseline")
  spdf0 = SpatialPoints(bathymetry[, c("plon", "plat")], proj4string=sp::CRS(proj4string) )

  raster_template = raster(extent(spdf0)) # +1 to increase the area
  res(raster_template) = resolution  # in units of crs = km
  crs(raster_template) = projection(spdf0) # transfer the coordinate system to the raster

  spobj = rasterize( bathymetry[, c("plon", "plat")], raster_template )
  if ( returntype=="raster") return(spobj)
  spobj = as(spobj, "SpatialPixelsDataFrame")

  spobj = as(spobj, returntype)
  spobj$StrataID = 1:length(spobj)  # row index
  row.names(spobj) = as.character(spobj$StrataID)

  return(spobj)
}


