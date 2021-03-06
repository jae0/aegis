
st_points_in_polygons = function( pts, polys, varname=NULL, method="sf_fast" ) {
  #// drop in replacement
  # using over from sp is defunct ... eg:
  # timings based upon full bathymetry lookup

  proj4string = projection_proj4string("lonlat_wgs84")

  if (method =="sp") {
    require(sp)
    # 8.5 min
    if (inherits(pts, c("data.frame", "matrix"))  ) pts = SpatialPoints( pts, sp::CRS(proj4string) )
    if (inherits(pts, "sf")) pts=as(pts, "Spatial")
    if (inherits(polys, "sf")) polys=as(polys, "Spatial")
    pts = spTransform(pts, CRS(proj4string) )
    polys = spTransform(polys, CRS(proj4string) )
    if (is.null(varname)) varname=1
    return( over( pts, polys )[,varname] )# match each datum to an area
  }

  # using sf and a slightly more complex call
  if (method=="sf_fast") {
    # 8.2 min
    require(sf)
    if (!inherits(pts, "sf")  ) {
      if (inherits(pts, "Spatial")) pts = as(pts, "sf")
      if (inherits(pts, "sfc"))     pts = st_as_sf(pts, "sf")
    }
    if (!inherits(polys, "sf")  ) {
      if (inherits(polys, "Spatial")) polys=as(polys, "sf")
      if (inherits(polys, "sfc")) polys=st_as_sf(polys)
    }

    if ( is.na(st_crs(polys) ) ) st_crs(polys) = st_crs(proj4string)
    if ( is.na(st_crs(pts) ) )   st_crs(pts)   = st_crs(polys)

    if (st_crs(polys) != st_crs(pts) ) polys = st_transform(polys, st_crs(pts) )

    if (is.null(varname)) varname="inside"
    if (!exists(varname, polys)) polys[,varname] = TRUE
    o = sapply( st_intersects( pts, polys), function(z) if (length(z)==0) NA_integer_ else z[1] )
    oo =  st_drop_geometry(polys) [ o, varname]
    return( oo )
  }

  # using sf and a slightly more complex call
  if (method=="sf_join") {
    # 35.7 min
    require(sf)
    if (!inherits(pts, "sf")  ) {
      if (inherits(pts, "Spatial")) pts = as(pts, "sf")
      if (inherits(pts, "sfc"))     pts = st_as_sf(pts, "sf")
    }
    if (!inherits(polys, "sf")  ) {
      if (inherits(polys, "Spatial")) polys=as(polys, "sf")
      if (inherits(polys, "sfc")) polys=st_as_sf(polys)
    }

    crs_internal = ifelse ( is.na(st_crs(polys)), st_crs( proj4string ),  st_crs(polys ) )
    if ( is.na(st_crs(pts) ) )   st_crs(pts)   = ifelse ( is.na(st_crs(polys)), crs_internal, st_crs(polys) )
    if ( is.na(st_crs(polys) ) ) st_crs(polys) = ifelse ( is.na(st_crs(pts)),   crs_internal, st_crs(pts) )

    if (st_crs(polys) != st_crs(pts) ) polys = st_transform(polys, st_crs(pts) )
    if (is.null(varname)) varname="inside"
    if (!exists(varname, polys)) polys[,varname] = TRUE
    polys = polys[,varname]
    return( st_join( pts, polys)[,varname] )
  }

  if (0) {
    # example
    crs_lonlat = st_crs( projection_proj4string("lonlat_wgs84") )
    M$AUID = st_points_in_polygons(
      pts=st_as_sf( M, coords=c("lon","lat"), crs=crs_lonlat ),
      polys=st_transform(sppoly, crs=crs_lonlat )[, "AUID"],
      varname="AUID"
    )
  }
}
