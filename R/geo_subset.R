
geo_subset = function( spatial_domain, Z ) {

  #\\ filter locations based upon depths and location of spatial_domain
  #\\ most are in planar coords (so require plon, plat and z {depth} )

  # trim to extents
  ps = spatial_parameters( spatial_domain=spatial_domain ) # obtain internal projection params
  corners =  rbind(
    c( ps$corners$lon[1], ps$corners$lat[2] ),
    c( ps$corners$lon[2], ps$corners$lat[2] ),
    c( ps$corners$lon[2], ps$corners$lat[1] ),
    c( ps$corners$lon[1], ps$corners$lat[1] ),
    c( ps$corners$lon[1], ps$corners$lat[2] )
  )
  bnd = (
    st_multipoint( corners )
    %>% st_sfc( crs=st_crs(projection_proj4string("lonlat_wgs84")) )
    %>% st_cast("POLYGON" )
    %>% st_make_valid()
  )

  if (!inherits("sf", Z)) {
    if (exists("lon", Z)) {
      Z = sf::st_as_sf( Z, coords = c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84"))  )
    } else if (exists("plon", Z)) {
      Z = sf::st_as_sf( Z, coords = c("plon","plat"), crs=st_crs(ps$aegis_proj4string_planar_km) )
    } else {
      Z = sf::st_as_sf( Z, coords = c(1,2), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
    }
  }

  if ( spatial_domain %in% c( "canada.east.superhighres", "canada.east.highres", "canada.east" ) ) {
    inside = which( st_points_in_polygons( Z, st_transform(bnd, crs=st_crs(Z) ) ) & Z$z < 5000 & Z$z > 0 )
  }

  if ( spatial_domain == "SSE.mpa" ) {
    inside = which( st_points_in_polygons( Z, st_transform(bnd, crs=st_crs(Z) )  ) & Z$z < 2000 & Z$z > 0 )
  }

  if ( spatial_domain =="SSE" ) {
    inside = which( st_points_in_polygons( Z, st_transform(bnd, crs=st_crs(Z) )  ) & Z$z <  800 & Z$z > 0 )
  }

  if ( spatial_domain == "snowcrab" ) {
    #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset with the following modifications:
    inside = which( st_points_in_polygons( Z, st_transform(bnd, crs=st_crs(Z) )  ) & Z$z < 350 & Z$z > 10 )
  }
  return(inside)
}

