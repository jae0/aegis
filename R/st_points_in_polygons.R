
st_points_in_polygons = function( pts, polys, varname=NULL, method="sp_direct" ) {
  #// drop in replacement
  # using over from sp is defunct ... eg:
  # timings based upon full bathymetry lookup

  proj4string = projection_proj4string("lonlat_wgs84")

  if (method =="sp_direct") {
    # no varname method
    require(sp)
    require(sf)

    if (inherits(pts, "sf"))  {
      pts_crs = st_crs(pts)
      pts = st_coordinates(pts)
      polys = st_transform(polys, pts_crs)
      polys = st_coordinates(polys)
    } else if (inherits(pts, "Spatial")) {
      pts_crs = proj4string(pts)
      pts = sp::coordinates(pts)
      polys = spTransform(polys, pts_crs)
      polys = coordinates(polys)
    } else {
      # coords expected lon/lat or plon/plats
    }
 
    o = point.in.polygon(pts[,1], pts[,2], polys[,1], polys[,2]) 

    out = rep(FALSE, length(o))
    out[which(o!=0)] = TRUE

    return( out ) # match each datum to an area
  }


  if (method =="sp_over") {
    require(sp)
    # 8.5 min
    if (inherits(pts, "sf")) {
      pts = as(pts, "Spatial")
    } else if (inherits(pts, c("data.frame"))  ) {
      pts = SpatialPoints( pts, sp::CRS(proj4string) )
    } else if (inherits(pts, c("matrix"))  ) {
      pts = SpatialPoints( pts, sp::CRS(proj4string) )
    }

    if (inherits(polys, "sf")) polys=as( polys, "Spatial")
    if (inherits(polys, "sfc")) polys=as( polys, "Spatial")
 
    pts = spTransform(pts, CRS(proj4string) )
    polys = spTransform(polys, CRS(proj4string) )
    
    out = over( pts, polys )
    
    if (is.null(varname)) {
      o = out[,1]  
      out = rep(FALSE, length(o))
      out[which(o==1)] = TRUE
    } else {
      out = out[, varname] 
    }

    return( out ) # match each datum to an area
  }


  # using sf and a slightly more complex call
  if (method=="sf_fast") {
    # 8.2 min
 
    require(sf)
    if (!inherits(pts, "sf")  ) {
      if (inherits(pts, "Spatial")) pts = as(pts, "sf")
      if (inherits(pts, "sfc"))     pts = st_as_sf(pts)
    }
    
    if (!inherits(polys, "sf")  ) {
      if (inherits(polys, "Spatial")) polys=as(polys, "sf")
      if (inherits(polys, "sfc")) polys=st_as_sf(polys) 
    }

    if ( is.na(st_crs(polys) ) ) st_crs(polys) = st_crs(proj4string)
    if ( is.na(st_crs(pts) ) )   st_crs(pts)   = st_crs(polys)

    if (st_crs(polys) != st_crs(pts) ) polys = st_transform(polys, st_crs(pts) )

    if (!is.null(varname)) {
      # default behaviour
      if (exists(varname, polys)) {
        o = sapply( st_intersects( pts, polys), function(z) if (length(z)==0) NA_integer_ else z[1] )
        out = st_drop_geometry(polys)[ o, varname]
        return(out)
      }
    }

    # special case when no varname is sent; make logical
    out = (lengths( st_intersects( pts, polys) ) > 0)  # using sparse list

    return(out)
  }


  if (method=="sf_join") { 
    # using sf and a slightly more complex call .. slow, do  not use

    # 35.7 min
    require(sf)
    if (!inherits(pts, "sf")  ) {
      if (inherits(pts, "Spatial")) pts = as(pts, "sf")
      if (inherits(pts, "sfc"))     pts = st_as_sf(pts) 
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

    if (!is.null(varname)) {
      # default behaviour
      if (exists(varname, polys)) {
      polys = polys[,varname]
      out = st_join( pts, polys, join = st_within)
      out = st_drop_geometry(out)[, varname]
      return(out)
      }
    }

    # special case when no varname is sent; make logical
    # out = st_join( pts, polys, join = st_within)
    # out = st_filter( pts, polys, .predicate = st_within)
    out = st_filter( pts, polys )
    
    out = st_drop_geometry(out)[, varname]
    o = rep(FALSE, length(out))
    o[which(out==1)] = TRUE
     
    return( o )
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
