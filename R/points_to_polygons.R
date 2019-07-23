

points_to_polygons = function( SPDF, SPDF.boundary=NULL, spbuffer=NULL ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data

  require(sp)
  require(rgeos)

  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    spbuffer = NULL
    proj4string=NULL
    res = points_to_polygons( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
  }

  rn0 = row.names(SPDF)  # store for end
  SPDF$uid_internal = 1:nrow(SPDF)
  row.names(SPDF) = SPDF$uid_internal

  xy = try( coordinates(SPDF) )
  if ( ("try-error" %in% class(xy)) ) stop( "Coordinates were not specified in data object?")
  rownames(xy) = SPDF$uid_internal

  SP = tessellate(xy) # via voronoi
    # plot(SP)


  if (is.null(spbuffer)) {
    drange = range( c( diff(range( xy[,1] )), diff(range(xy[,2] )) ) )
    spbuffer =  floor( min(drange)/ 25 ) + 1L
  }

  # define boundary of points if no boundary -- could also use convex hull ...
  if (is.null(SPDF.boundary)) {
    SPDF.boundary = gBuffer( gUnaryUnion( gBuffer( SPDF, width=spbuffer, byid=TRUE) ), width=spbuffer)
    # plot(SPDF.boundary)
    row.names(SPDF.boundary) = ""
  }

  SP = gIntersection(  SPDF.boundary, SP, byid=TRUE ) # crop
  row.names(SP) =  gsub( "[[:space:]]", "", row.names(SP) )
  # row.names(SP) = SPDF$uid_internal .. equivalent as order is not touched .. but just in case
  # SPDF = SPDF[ order(as.numeric(SPDF$uid_internal)), ]
  # SP = SP[order(as.numeric(row.names(SP)))]

  O = SpatialPolygonsDataFrame(SP, data=as.data.frame(SPDF), match="uid_internal" )
  row.names(O) = rn0
  O$uid_internal = NULL


  if (0) {
    # make sure it is OK
    plot( coordinates(SPDF)[,2] ~ coordinates(O)[,2] )
    plot( coordinates(SPDF)[,1] ~ coordinates(O)[,1] )
    plot( SPDF$zinc ~ O$zinc )

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
    # mypalette = rev( heat.colors( 150 ) )
    #mypalette <- brewer.pal(8, "YlOrRd")

    SPDF$logz = log( SPDF$zinc )
    O$logz = log( O$zinc )

    spplot( SPDF, "zinc", col.regions=mypalette )
    spplot( O, "zinc", col.regions=mypalette )
  }

  return(O)
}
