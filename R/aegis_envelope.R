aegis_envelope = function( xy, method="non_convex_hull", spbuffer=NULL, returntype="SpatialPolygons", proj4string=NULL, hull_multiplier=1 ) {

  # obtain boundary of a bunch of points .. expect spatiial points
  drange = range( c( diff(range( xy[,1] )), diff(range(xy[,2] )) ) )
  spbuffer_default =  floor( min(drange)/ 25 )

  if (is.null(spbuffer)) {
    spbuffer = spbuffer_default
    # message( "spbuffer not set, using spbuffer=", spbuffer)
  }

  if (spbuffer < spbuffer_default / 4 ) {
    spbuffer = spbuffer_default
    # message( "spbuffer very low. Setting initial spbuffer =", spbuffer/4)
  }

  # define boundary of points if no boundary -- could also use convex hull ...
  if (method=="gBuffer") {
    xy = as.data.frame(xy)
    names(xy) = c("x", "y")
    coordinates(xy) = ~ x+y
    sp::proj4string(xy) = CRS("+init=epsg:28992")
    bnd = gBuffer( gUnaryUnion( gBuffer( M, width=spbuffer, byid=TRUE) ), width=spbuffer)
    # plot(bnd)
  }

  if (method=="concave.hull") {
    v = concave.hull( xy, ub=spbuffer*hull_multiplier)
    if ( any( !is.finite(v) )) next()
    w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
    bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string) )
    bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
    # plot(bnd)
  }

  if (method=="non_convex_hull") {
    v = non_convex_hull( xy, alpha=spbuffer*hull_multiplier  )
    if ( any( !is.finite(v) )) next()
    w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
    bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string) )
    bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
      # plot(bnd)
  }


  if (returntype=="xy") {
    bnd = coordinates(bnd)
    attr( bnd, "proj4string" ) =  proj4string
  } else {
    bnd = as(bnd, returntype )
  }

  return (bnd)

}