aegis_envelope = function( xy, method="non_convex_hull", spbuffer=NULL, returntype="sf", proj4string=NULL, hull_lengthscale=NULL ) {

  # obtain boundary of a bunch of points .. expect spatial points
  drange = range( c( diff(range( xy[,1] )), diff(range(xy[,2] )) ) )
  spbuffer_default =  trunc( min(drange)/ 25 )

  if (is.null(spbuffer)) {
    spbuffer = spbuffer_default
    # message( "spbuffer not set, using spbuffer=", spbuffer)
  }

  if (spbuffer < spbuffer_default / 4 ) {
    spbuffer = spbuffer_default
    # message( "spbuffer very low. Setting initial spbuffer =", spbuffer/4)
  }

  if (is.null(hull_lengthscale)) {
    hull_lengthscale =  drange / 100   
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

  if (method=="concave.hull.sp") {
    v = concave.hull( xy, ub=hull_lengthscale)
    if ( any( !is.finite(v) )) next()
    w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
    bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string) )
    bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
    # plot(bnd)
  }

  if (method=="non_convex_hull.sp") {
    v = non_convex_hull( xy, lengthscale=hull_lengthscale  )
    if ( any( !is.finite(v) )) next()
    w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
    bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string) )
    bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
      # plot(bnd)
  }

  if (method=="concave.hull") {
    v = concave.hull( xy, ub=hull_lengthscale)
    if ( any( !is.finite(v) )) next()
    bnd = (
#      st_sfc( st_multipoint( xy ), crs=st_crs(proj4string) )
      st_sfc( st_multipoint( xy ) )
      %>% st_buffer(spbuffer)
      %>% st_union()
      %>% st_buffer(spbuffer)
      %>% st_simplify()
      %>% st_buffer(0)
      %>% st_cast("POLYGON" )
      %>% st_make_valid()
    )
    # plot(bnd)
  }

  if (method=="non_convex_hull") {
    v = non_convex_hull( xy, lengthscale=hull_lengthscale  )
    if ( any( !is.finite(v) )) next()
    bnd = (
#      st_sfc( st_multipoint( xy ), crs=st_crs(proj4string) )
      st_sfc( st_multipoint( xy ) )
      %>% st_buffer(spbuffer)
      %>% st_union()
      %>% st_buffer(spbuffer)
      %>% st_simplify()
      %>% st_buffer(0)
      %>% st_cast("POLYGON" )
      %>% st_make_valid()
    )
      # plot(bnd)
  }


  if (returntype=="xy") {
    bnd =st_coordinates(bnd)
    attr( bnd, "proj4string" ) =  proj4string
  } else if (returntype!="sf"){
    bnd = as( bnd, returntype )
  } else {
    #nothing to do
  }

  return (bnd)

}