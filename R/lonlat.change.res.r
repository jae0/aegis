 
  
  lonlat.change.res = function( ss, res=2 ) {
    # res is resolution in minutes
    
    ss$lon.min = (ss$lon - trunc( ss$lon ) ) *60 
    ss$lon.min = trunc( ss$lon.min / res ) * res 
    ss$lon = round( trunc(ss$lon) + ss$lon.min / 60 , 2 )

    ss$lat.min = (ss$lat - trunc( ss$lat ) ) *60 
    ss$lat.min = trunc( ss$lat.min / res ) * res 
    ss$lat = round( trunc(ss$lat) + ss$lat.min / 60, 2)
    return (ss)
  }
  

