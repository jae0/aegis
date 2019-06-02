
  applySummary = function( f, newnames=NULL, ... ) {

    # return mean, sd, count by category

    cnames= c("id", "x", "w" )

    if (is.null( newnames )) newnames=names(f)

    nv = ncol(f)
    names(f) = cnames[1:nv]

    f$id = as.factor( f$id )
    f$x = as.numeric( f$x )

    o = which( is.finite( f$x) )
    if ( length(o) == 0 ) return( "No data?")
    f = f[o,]

    # means

    if (nv==2 ) {
      # no weighting factor .. simple means

      l0 = 1/(median(f$x))
      out0 = as.data.frame( xtabs( f$x * l0 ~ f$id, na.action=na.omit) / l0 , stringsAsFactors=FALSE )
      names( out0) = c("id", "sumx" )

      out1 = as.data.frame( xtabs( ~ f$id, na.action=na.omit) , stringsAsFactors=FALSE )
      names( out1) = c("id", "n" )

      out = merge( out0, out1, by="id", all=TRUE, sort=FALSE )
      out$mean = out$sumx / out$n
    }

    if( nv==3 ) {
      # has a weighting factor .. weighted average

      o = which( is.finite( f$x + f$w) )
      if ( length(o) == 0 ) return( "No data?")
      f = f[o,]

      f$xw = as.numeric( f$x * f$w )

      l0 = 1 / median( f$xw, na.rm=TRUE ) # a scaling factor to help avoid overflow errors
      l1 = 1 / median( f$w,  na.rm=TRUE )

      out0 = as.data.frame( xtabs( f$xw * l0 ~ f$id, na.action=na.omit) / l0 , stringsAsFactors=FALSE )
      names( out0) = c("id", "sumxw" )

      out1 = as.data.frame( xtabs( f$w *l1 ~ f$id, na.action=na.omit) / l1 , stringsAsFactors=FALSE )
      names( out1) = c("id", "sumw" )

      out = merge( out0, out1, by="id", all=TRUE, sort=FALSE )
      out$mean = out$sumxw / out$sumw
    }

    # SD
    rowindex = 1:nrow(f)
    sds = aggregate(rowindex ~ id, f, function(i) sd(f$x[i], na.rm=TRUE ), na.action=na.omit ) #arithmetic mean
    names(sds) = c("id", "sd")

    out = merge(out, sds, by="id", all.x=TRUE, all.y=TRUE)

    out = out[, c("id", "mean", "sd", "n")]
    names(out) = c( newnames[1], paste(newnames[2],  c( "mean", "sd", "n"), sep=".") )

    return( out )
  }
