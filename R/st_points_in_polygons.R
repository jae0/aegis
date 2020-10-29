st_points_in_polygons = function( pts, polys, proj4string=projection_proj4string("lonlat_wgs84"), varname=NULL, method="sf_fast" ) {
  #// drop in replacement
  # using over from sp is defunct ... eg:
  # timings based upon full bathymetry lookup
  if (method =="sp") {
    require(sp)
    # 8.5 min
    if (inherits(pts, c("data.frame", "matrix"))  ) pts = SpatialPoints( pts, sp::CRS(proj4string) )
    if (inherits(pts, "sf")) pts=as(pts, "Spatial")
    if (inherits(polys, "sf")) polys=as(polys, "Spatial")
    if (CRS(pts) != CRS(proj4string) ) pts = spTransform(pts, CRS(proj4string) )
    if (CRS(polys) != CRS(proj4string) ) polys = spTransform(polys, CRS(proj4string) )
    if (is.null(varname)) varname=1
    spTransform(sppoly[,varname], sp::CRS(proj4string) )
    # pts = SpatialPoints( M[, c("lon", "lat")], sp::CRS(proj4string) )
    # polys = spTransform(sppoly, proj4string )
    return( over( pts, polys )[,varname] )# match each datum to an area
  }

  # using sf and a slightly more complex call
  if (method=="sf_fast") {
    # 8 min
    require(sf)
    if (inherits(pts, c("data.frame", "matrix"))  ) pts = st_as_sf( pts, coords = c(1,2), crs=sf::st_crs(proj4string) )
    if (inherits(pts, "Spatial")) pts=as(pts, "sf")
    if (inherits(polys, "Spatial")) polys=as(polys, "sf")
    if (st_crs(pts) != st_crs(proj4string) ) pts = st_transform(pts, st_crs(proj4string) )
    if (st_crs(polys) != st_crs(proj4string) ) polys = st_transform(polys, st_crs(proj4string) )
    if (is.null(varname)) varname=1
    polys = polys[,varname]
    return( sapply( st_intersects( pts, polys), function(z) if (length(z)==0) NA_integer_ else z[varname] ) )
  }

  # using sf and a slightly more complex call
  if (method=="sf_slow") {
    # 35.7 min
    require(sf)
    if (inherits(pts, c("data.frame", "matrix"))  ) pts = st_as_sf( pts, coords = c(1,2), crs=sf::st_crs(proj4string) )
    if (inherits(pts, "Spatial")) pts=as(pts, "sf")
    if (inherits(polys, "Spatial")) polys=as(polys, "sf")
    if (st_crs(pts) != st_crs(proj4string) ) pts = st_transform(pts, st_crs(proj4string) )
    if (st_crs(polys) != st_crs(proj4string) ) polys = st_transform(polys, st_crs(proj4string) )
    if (is.null(varname)) varname=1
    polys = polys[,varname]
    return( st_join( pts, polys)[,varname] )
  }

}
