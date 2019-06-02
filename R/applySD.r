
  applySD = function( f, method="slow", newnames=NULL ) {
    cnames= c("id", "x")

    if (is.null( newnames )) newnames=names(f)

    nv = ncol(f)
    names(f) = cnames[1:nv]

    if (method=="slow") {
        r = by( f, f$id, with, sd( x, na.rm=TRUE ))
        out = data.frame( id=names(r), x=as.numeric(as.vector(r)) , stringsAsFactors=FALSE )
    }

    if (method=="compact_but_slow") {
      rowindex = 1:nrow(f)
      out = aggregate(rowindex ~ id, f, function(i) sd(f$x[i], na.rm=TRUE ), na.action=na.omit ) #arithmetic mean
    }

    names(out) = newnames[1:2]
    return( out )
  }
