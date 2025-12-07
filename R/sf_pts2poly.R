
sf_pts2poly = function(x) {
  require(sf)
  out = (
    st_multipoint( as.matrix(x) )
    %>% st_sfc( crs=st_crs(projection_proj4string("lonlat_wgs84")) )
    %>% st_cast("POLYGON" )
    %>% st_make_valid()
  )
  return(out)
}