
spatial_grid = function( p, DS="planar.grid", nm=NULL ) {
  #\\ grid locations

  if (DS=="planar.grid") {
    plon = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    plat = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    g = expand.grid( plon, plat, KEEP.OUT.ATTRS=FALSE)
    if (is.null(nm)) nm=c("plon", "plat")
    names( g ) = nm
    return(g)
  }

  if (DS=="lonlat.grid") {
    lons = seq(min(p$corners$lon), max(p$corners$lon), by=p$dres)
    lats = seq(min(p$corners$lat), max(p$corners$lat), by=p$dres)
    g = expand.grid( lons, lats, KEEP.OUT.ATTRS=FALSE)
    if (is.null(nm)) nm=c("lon", "lat")
    names( g ) = nm
    return(g)
  }

  if (DS=="planar.coords") {
    if (is.null(nm)) nm=c("plon", "plat")
    out = list()
    out[[nm[1]]]  = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    out[[nm[2]]]  = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    return(out)
  }

  if (DS=="lonlat.coords") {
    if (is.null(nm)) nm=c("lon", "lat")
    out = list()
    out[[nm[1]]]  = seq(min(p$corners$lon), max(p$corners$lon), by=p$dres)
    out[[nm[2]]]  = seq(min(p$corners$lat), max(p$corners$lat), by=p$dres)
    return(out)
  }

}

