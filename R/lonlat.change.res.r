

  lonlat.change.res = function( ss, res=2 ) {
    # res is resolution in minutes

    ss$lon.min = (ss$lon - aegis_floor( ss$lon ) ) *60
    ss$lon.min = aegis_floor( ss$lon.min / res ) * res
    ss$lon = round( aegis_floor(ss$lon) + ss$lon.min / 60 , 2 )

    ss$lat.min = (ss$lat - aegis_floor( ss$lat ) ) *60
    ss$lat.min = aegis_floor( ss$lat.min / res ) * res
    ss$lat = round( aegis_floor(ss$lat) + ss$lat.min / 60, 2)
    return (ss)
  }


