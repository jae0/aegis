
  applySum = function( f, method="fast", newnames=NULL ) {
    cnames= c("id", "x", "w" )

    if (is.null( newnames )) newnames=names(f)

    if( is.vector(f) ) {

      out = as.data.frame( xtabs( ~ f) , stringsAsFactors=FALSE  )

    } else {

      # data.frame metod

      nv = ncol(f)
      names(f) = cnames[1:nv]

      if( nv==3 ) f$x = f$x* f$w

      o = which( is.finite( f$x) )
      if ( length(o) == 0 ) return( "No data?")
      f = f[o,]

      if (method=="slow") {
        r = by( f, f$id, with, sum( x, na.rm=TRUE ))
        out = data.frame( id=names(r), x=as.numeric(as.vector(r)) , stringsAsFactors=FALSE )

      } else if (method=="fast") {

        f$id = as.factor( f$id )
        l0 = 1 / median( f$x, na.rm=TRUE ) # a scaling factor to help avoid overflow errors
        f$x = as.numeric( f$x * l0)
        out = as.data.frame( xtabs( f$x ~ f$id, na.action=na.omit) / l0, stringsAsFactors=FALSE )

      } else if (method=="compact_but_slow") {

        rowindex = 1:nrow(f)
        out = aggregate( rowindex ~ id, f, function(i) sum( f$x[i], na.rm=TRUE ), na.action=na.omit ) #

      }
    }

    names(out) = newnames[1:2]
    return( out )
  }
