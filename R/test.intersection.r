
  test.intersection = function(pfocus, hull, p.test ) {
    
    out = NULL
    nch = nrow(hull)
    no.segments = nch-3

    if (nch < 4)  return(  as.matrix( t( p.test[1,] )) )

    for ( i in 1:nrow( p.test ) ){
     
      i.test = 1

      for (ns in 1:no.segments) {
     
        pt1 = nch-ns
        pt2 = pt1 - 1

        p1a = hull[ pt1, ]
        p1b = hull[ pt2, ]
      
        xrange = range( c(p1a[1], p1b[1] ) ) 
        xrange2 = range( pfocus[1], p.test[i,1] )
        
        p.intersection = try( line.intersection.2d ( p1a, p1b, pfocus, p.test[i,] ) )
        
        if ( is.null( p.intersection ) ) {
          break()
          # next()
        }
        if ( any(!is.finite(p.intersection)) ) {
          message("this should not happen .. some defaults have changed ?")
          browser()
          stop()
        }

        if ( p.intersection[1] >= xrange[1] & p.intersection[1] <= xrange[2] & 
             p.intersection[1] >= xrange2[1] & p.intersection[1] <= xrange2[2] ) {
          i.test = i.test * 0  
        }

        if (i.test == 0) break()
     }

     if ( i.test == 1 )  {
        out =  as.matrix( t( p.test[i,] ) ) 
        break()
     }
  
    }

    return (out)      
  }



