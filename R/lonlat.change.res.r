

  lonlat.change.res = function( ss, res=2 ) {
    # res is resolution in minutes

    ss$lon.min = (ss$lon - floor( ss$lon ) ) *60
    ss$lon.min = floor( ss$lon.min / res ) * res
    ss$lon = round( floor(ss$lon) + ss$lon.min / 60 , 2 )

    ss$lat.min = (ss$lat - floor( ss$lat ) ) *60
    ss$lat.min = floor( ss$lat.min / res ) * res
    ss$lat = round( floor(ss$lat) + ss$lat.min / 60, 2)
    return (ss)
  }


