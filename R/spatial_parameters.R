
spatial_parameters = function( p=list(), ... ) {

  p = parameters_add(p, list(...) ) # add passed args to parameter list, priority to args

    if ( p$spatial_domain %in% c("snowcrab") ) {

        p$aegis_proj4string_planar_km =  projection_proj4string("utm20")
        p$dres = 1/60/4  # this is the 15 second grid from CHS  .. default use highest resolution
        p$pres = 1   # 1 km resolution!
        p$lon0=-66.1
        # p$lon1=-57.0
        p$lon1=-56.6
        p$lat0=42.8
        p$lat1=47.4
        p$psignif = 1

    } else if ( p$spatial_domain %in% c("SSE") ) {

        p$aegis_proj4string_planar_km =  projection_proj4string("utm20")
        p$dres = 1/60/4  # this is the 15 second grid from CHS  .. default use highest resolution
        p$pres = 1   # 1 km resolution!
        p$lon0=-68
        p$lon1=-56
        p$lat0=41
        p$lat1=48
        p$psignif = 1

    } else if ( p$spatial_domain %in% c("SSE.mpa") ) {

        p$aegis_proj4string_planar_km = projection_proj4string("utm20")
        p$dres = 1/60/4  # this is the 15 second grid from CHS  .. default use highest resolution
        p$pres = 1  # 1 km resolution
        p$lon0=-71
        p$lon1=-48
        p$lat0=37
        p$lat1=48
        p$psignif = 1
        p$boundary = "scotia.fundy.with.buffer"

    } else if ( p$spatial_domain=="canada.east") {

        p$aegis_proj4string_planar_km =  projection_proj4string("lambert.conic.canada.east")
        p$dres = 1/60/4  # this is the 15 second grid from CHS  .. ~ 0.25 km
        p$pres = 1  # km
        p$lon0=-72
        p$lon1=-52
        p$lat0=40
        p$lat1=50
        p$psignif = 1

    } else if ( p$spatial_domain %in% c("canada.east.highres")) {

        p$aegis_proj4string_planar_km =  projection_proj4string("lambert.conic.canada.east")
        p$dres = 1/60/4  # CHS is 15 arc second ~ 0.25 km
        p$pres = 0.5  # discretize to 0.5 km resolution
        p$lon0=-72
        p$lon1=-52
        p$lat0=40
        p$lat1=50
        p$psignif = 1

    } else if ( p$spatial_domain %in% c("canada.east.superhighres")) {

        p$aegis_proj4string_planar_km =  projection_proj4string("lambert.conic.canada.east")
        p$dres = 1/60/4  # CHS is 15 arc second ~ 0.25 km
        p$pres = 0.2  # discretize to 0.2 km resolution
        p$lon0=-72
        p$lon1=-52
        p$lat0=40
        p$lat1=50
        p$psignif = 1

    } else {

        message( "The spatial_domain was not recognised:")
        message( p$spatial_domain )
        message( "Assuming this is manual mode and you are constructing your own list .. you are on your own ...")

     }

    p$nlons = trunc( diff(range(c(p$lon0,p$lon1)))/p$dres) + 1L
    p$nlats = trunc( diff(range(c(p$lat0,p$lat1)))/p$dres) + 1L
    corners = data.frame(lon=c(p$lon0,p$lon1), lat=c(p$lat0,p$lat1))
    corners = lonlat2planar( corners, proj.type=p$aegis_proj4string_planar_km )
    corners$plon = round( corners$plon, p$psignif)  # this matches the p$pres value of x km resolution
    corners$plat = round( corners$plat, p$psignif)  # this matches the p$pres value of x km resolution
    p$corners=corners

    p$plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    p$plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    plons = seq(min(p$corners$plon), max(p$corners$plon), by=p$pres)
    plats = seq(min(p$corners$plat), max(p$corners$plat), by=p$pres)
    p$nplons = length(plons)
    p$nplats = length(plats)
    p$origin = c(min(p$corners$plon), min(p$corners$plat ))
    p$gridparams = list( dims=c(p$nplons, p$nplats), origin=p$origin, res=c(p$pres, p$pres) ) # used for fast indexing and merging


  return(p)
}
