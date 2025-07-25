
  applyMean = function( f, method="fast", newnames=NULL, ... ) {

    message("This functionality is deprecated ... use data.table() instead as it is faster and more compact.")

    cnames= c("id", "x", "w" )

    if (is.null( newnames )) newnames=names(f)

    nv = ncol(f)
    names(f) = cnames[1:nv]


    if (method=="slow") {
      if (nv==2) {
        r = by( f, f$id, with, mean( x, na.rm=TRUE, ... ))
        out = data.frame( id=names(r), x=as.numeric(as.vector(r)) , stringsAsFactors=FALSE )
      }
      if (nv==3 ) {
        r = by( f, f$id, with, weighted.mean( x, w, na.rm=TRUE, ... ))
        out = data.frame( id=names(r), x=as.numeric(as.vector(r)) , stringsAsFactors=FALSE )
      }
    }


    if (method=="compact_but_slow") {
      rowindex = 1:nrow(f)
      if (nv==2) {
        out = aggregate(rowindex ~ id, f, function(i) mean(f$x[i], na.rm=TRUE ), na.action=na.omit ) #arithmetic mean
      }
      if (nv==3 ) {
        out = aggregate(rowindex ~ id, f, function(i) weighted.mean(f$x[i], f$w[i], na.rm=TRUE ), na.action=na.omit ) # weighted mean
      }
    }


    if (method=="fast") {
      if (nv==2 ) {
        # no weighting factor .. simple means
        o = which( is.finite( f$x) )
        if ( length(o) == 0 ) return( "No data?")
        f = f[o,]

        f$id = as.factor( f$id )
        f$x = as.numeric( f$x )
        l0 = 1/(median(f$x))
        out0 = as.data.frame( xtabs( f$x * l0 ~ f$id, na.action=na.omit) / l0 , stringsAsFactors=FALSE )
        names( out0) = c("id", "sumx" )

        out1 = as.data.frame( xtabs( ~ f$id, na.action=na.omit) , stringsAsFactors=FALSE )
        names( out1) = c("id", "n" )

        out = merge( out0, out1, by="id", all=TRUE, sort=FALSE )
        out$res = out$sumx / out$n
        out = out[ ,c( "id", "res" )]
      }

      if( nv==3 ) {
        # has a weighting factor .. weighted average

        o = which( is.finite( f$x + f$w) )
        if ( length(o) == 0 ) return( "No data?")
        f = f[o,]

        f$id = as.factor( f$id )
        f$x = as.numeric( f$x )
        f$xw = as.numeric( f$x * f$w )

        l0 = 1 / median( f$xw, na.rm=TRUE ) # a scaling factor to help avoid overflow errors
        l1 = 1 / median( f$w,  na.rm=TRUE )

        out0 = as.data.frame( xtabs( f$xw * l0 ~ f$id, na.action=na.omit) / l0 , stringsAsFactors=FALSE )
        names( out0) = c("id", "sumxw" )

        out1 = as.data.frame( xtabs( f$w *l1 ~ f$id, na.action=na.omit) / l1 , stringsAsFactors=FALSE )
        names( out1) = c("id", "sumw" )

        out = merge( out0, out1, by="id", all=TRUE, sort=FALSE )
        out$res = out$sumxw / out$sumw
        out = out[ ,c( "id", "res" )]

      }
    }

    names(out) = newnames[1:2]
    return( out )
  }
