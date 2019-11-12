
aegis_db_spatial_object = function( spatial_domain="SSE", proj4string="+proj=utm +ellps=WGS84 +zone=20 +units=km", areal_units_resolution_km=1, returntype="SpatialPixelsDataFrame" ) {
  require(sp)
  require(raster)
  # return base layout of aegis grids as a Spatial* object
  # bathymetry is the basis of all grids

  pn = spatial_parameters( spatial_domain=spatial_domain )

  bathymetry = bathymetry.db( p=pn, DS="complete" )
  bathymetry = geo_subset( spatial_domain=spatial_domain, Z=bathymetry )

  spdf0 = SpatialPoints(bathymetry[, c("plon", "plat")], proj4string=sp::CRS(proj4string) )

  raster_template = raster(extent(spdf0)) # +1 to increase the area
  res(raster_template) = areal_units_resolution_km  # in units of crs (which should be in  km)
  crs(raster_template) = projection(spdf0) # transfer the coordinate system to the raster

  spobj = rasterize( bathymetry[, c("plon", "plat")], raster_template, field=bathymetry$z )
  if ( returntype=="raster") return(spobj)
  spobj = as(spobj, "SpatialPixelsDataFrame")

  spobj = as(spobj, returntype)
  spobj$StrataID = 1:length(spobj)  # row index
  row.names(spobj) = as.character(spobj$StrataID)

  return(spobj)
}


