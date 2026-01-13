point_to_bbox = function( pt, plot_crs=st_crs(projection_proj4string("lonlat_wgs84") ) ) {

  bb = data.table( lon=pt$lon, lat=pt$lat ) 
  bb = st_as_sf( bb, coords= c("lon", "lat") )
  st_crs(bb) = st_crs(projection_proj4string("lonlat_wgs84"))

  bb = st_bbox(st_transform( bb, plot_crs ))
  bb = list(
    x = bb[c(1,3)],
    y = bb[c(2,4)]
  )

  return(bb)
}
