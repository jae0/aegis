
  concave.hull = function( xy, k=5, ub=NULL, plot=FALSE, random.start=FALSE ) {

    if (k < 3) return (NULL)
    pts = as.matrix( xy )
    dimnames(pts) =NULL
    pts = pts[ ! duplicated( pts) , ]
    if (is.null(ub)) {
      dx = diff(range(xy[,1]))
      dy = diff(range(xy[,2]))
      dd = max( dx, dy )
      ub = dd / 8
    }

    nsets = nrow (pts)
    if ( nsets < 3 ) return ( NULL )
    if ( nsets == 3) return( pts )

    # initiate list with the first few points
    convex.hull <- spatstat::convexhull.xy( pts )

    p0i = which.min( convex.hull$bdry[[1]]$y )
    if (random.start) {
      p0i = trunc(runif(1) * length(convex.hull$bdry[[1]]$y) ) + 1
    }
    pts.start = as.matrix( cbind( convex.hull$bdry[[1]]$x[ p0i ], convex.hull$bdry[[1]]$y[ p0i ] ) )
    hull = p0 = pts.start
    to.remove = which( p0[,1] == pts[,1] & p0[,2] == pts[,2] )
    if (length( to.remove) > 0 ) pts = pts[ - to.remove ,]

    finished = F
    while ( ! finished ) {
      if (nrow(hull)==3) pts = rbind( pts.start, pts)  # return pts.start after a few steps
      pts.new=NULL
      pts.new = nearest.sorted.angles( pts, hull, k, eps=pi/10, ub=ub ) # eps determines the acute angle solutions to ignore
      if (is.null(pts.new) ) {
        kadd = k
        while (is.null(pts.new) ) {
          kadd = kadd + 1
          pts.new = nearest.sorted.angles( pts, hull, k=kadd, eps=pi/10, ub=ub )
          if ((kadd-k) > 10) stop()
        }
      }
      if (pts.new[,1] == pts.start[,1] & pts.new[,2] == pts.start[,2]) finished = T
      p0 = pts.new
      hull = rbind( hull, p0 )
      to.remove = which( p0[,1] == pts[,1] & p0[,2] == pts[,2] )
      if (length( to.remove) > 0 ) pts = pts[ - to.remove ,]
#      lines(hull[,1], hull[,2])
    }

    sa = signif(splancs::areapl(hull),3)
    densit = signif( nsets/splancs::areapl(hull),3)

    print( paste("Surface area:", sa))
    print( paste("Mean density:", densit))
    print( paste("Neighbourhood radius:", ub))
    print( "Check the plot and modify ub to alter the complexity of the outline." )

    if (plot) {
      plot( xy, pch=20, col="gray",
        sub = paste("Criteria:", k, "neighbours and", ub, "km radius", "\n No. sets =", nsets, "; SA = ", sa, "km^2 \n Density = ", densit, "stations / km^2" ),
        ylab="", xlab="" )
      lines(hull, lwd=2)
    }

    return (hull)
  }


