    boundingbox = function( x, y, buffer=0, returnvalue="sp", proj4string=CRS("+proj=longlat +datum=WGS84") ) {
      
      xr = range(x, na.rm=TRUE)
      yr = range(y, na.rm=TRUE)
 
      if ( buffer != 0 ) {
        xv = diff(xr) * buffer
        yv = diff(yr) * buffer
        xr[1] = xr[1] - xv
        xr[2] = xr[2] + xv
        yr[1] = yr[1] - yv
        yr[2] = yr[2] + yv
      }
      out = rbind( 
        c( xr[1], yr[2] ),
        c( xr[2], yr[2] ),
        c( xr[2], yr[1] ),
        c( xr[1], yr[1] ),
        c( xr[1], yr[2] ) 
      )
    
      if (returnvalue=="sp") out = SpatialPolygons( list(Polygons(list(Polygon(out)),"bbox")), proj4string=proj4string  )      

      return(out)
    }
