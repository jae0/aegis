aegis_db_lookup = function( X, lookupvars, xy_vars=c("lon", "lat"), time_var="timestamp", spatial_domain="SSE", yrs = c(1950:lubridate::year(lubridate::now() ) ) ) {

  # wrapper around aegis_lookup_stmv to make in-situ lookup from stmv simpler
  # spatial_domain = "SSE" is the default and really the only supported projection ..
  # others are possible but 10.surveys and associated depndencies need to be created as well

  if (!exists(xy_vars[1], X) | !exists(xy_vars[2], X) ) stop( "xy_vars not found in input data" )

  p = spatial_parameters( spatial_domain=spatial_domain )  # default (= only supported resolution of 0.2 km discretization)  .. do NOT change
  p$yrs = yrs
  p = temporal_parameters(p=p, aegis_dimensionality="space-year" )

  p$stmv_variables = list(
    LOCS=c("plon", "plat"),
    TIME="tiyr"
  )

  p = aegis_parameters(p=p, DS="stmv") # generics

  if ( xy_vars[1] != "lon" ) X$lon = X[,xy_vars[1]]
  if ( xy_vars[2] != "lat" ) X$lat = X[,xy_vars[2]]

  # rename in case of variable names colliding
  if (exists("plon", X)) X$plon_0 = X$plon
  if (exists("plat", X)) X$plat_0 = X$plat

  X = lonlat2planar( X, proj.type=p$aegis_proj4string_planar_km )  # SSE crs is "+proj=utm +ellps=WGS84 +zone=20 +units=km"

  X_new = aegis_lookup_stmv( DS="all", p=p, locs=X[,c("plon", "plat")], timestamp=X[,time_var], varnames=lookupvars )
  newvars = setdiff( lookupvars, names(X) )
  for ( vn in setdiff( lookupvars, newvars ) ) {
    i = which( !is.finite(X[,vn]) )
    if (length(i) > 0) X[i,vn] = X_new[i,vn]
  }
  X = cbind( X, X_new[, newvars])
  return(X)
}
