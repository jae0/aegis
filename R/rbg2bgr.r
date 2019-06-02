
  rbg2bgr = function ( rgb ) {
    rgb = as.character( rgb) 
    if ( nchar( rgb) == 7 ) {
      r = substr( rgb, 2,3 )
      g = substr( rgb, 4,5 )
      b = substr( rgb, 6,7 ) 
    } else if (nchar( rgb) == 6 ) {
      r = substr( rgb, 1,2 )
      g = substr( rgb, 3,4 )
      b = substr( rgb, 5,6 )
    }
    bgr = paste( "#", b,g,r, sep="" ) 
    return (bgr)
  }


