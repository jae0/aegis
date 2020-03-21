

aegis_mesh = function( SPDF, SPDF_boundary="default", spbuffer=NULL, k=5, resolution=100, output_type="polygons" ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data

  require(sp)
  require(rgeos)


  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")
    SPDF= meuse

    spbuffer = NULL
    k =5
    resolution = 100
    output_type = "polygons"

    res = aegis_mesh( SPDF=meuse) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50, output_type="grid" )
  }

  message( "'aegis_mesh' expects the projection to be in planar coordinates and also the same units as the resolution.")

  proj4string0 = sp::proj4string( SPDF )

  if (is.na(proj4string0)) stop("Input data does not have a projection")


  if ( output_type=="grid" ) {
    # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data
    require(raster)
    require(fasterize)
    raster_template = raster(extent(sp::bbox(SPDF))) # +1 to increase the area
    res(raster_template) = resolution
    crs(raster_template) = sp::CRS(proj4string0) # projection(SPDF) # transfer the coordinate system to the raster
    rast = rasterize(SPDF, raster_template, names(SPDF), fun=mean, na.rm=TRUE) # not meaningful fir factors
    O = as(rast, "SpatialGridDataFrame")
    O = spTransform( O, sp::CRS(proj4string0) )  # revert to input coordinate system
    return(O)
  }


  if ( output_type=="polygons" ) {

    rn0 = row.names(SPDF)  # store for end
    SPDF$uid_internal = as.character( 1:nrow(SPDF) )
    row.names(SPDF) = SPDF$uid_internal

    xy = try( coordinates(SPDF) )
    if ( ("try-error" %in% class(xy)) ) stop( "Coordinates were not specified in data object?")
    rownames(xy) = SPDF$uid_internal

    SP = tessellate(xy) # via voronoi
    # plot(SP)

    if (is.null(spbuffer)) {
      drange = range( c( diff(range( xy[,1] )), diff(range(xy[,2] )) ) )
      spbuffer =  floor( min(drange)/ 25 ) + 1L
      message( "spbuffer not set, using spbuffer=", spbuffer)
    }

    # define boundary of points if no boundary -- could also use convex hull ...
    if (SPDF_boundary=="gBuffer") {
      SPDF_boundary = gBuffer( gUnaryUnion( gBuffer( SPDF, width=spbuffer, byid=TRUE) ), width=spbuffer)
      # plot(SPDF_boundary)
    }

    if (SPDF_boundary=="concave.hull") {
      SPDF_boundary = concave.hull( xy, ub=spbuffer*3)
    }

    if (SPDF_boundary=="non_convex_hull") {
      v = non_convex_hull( xy, alpha=spbuffer*4  )
      w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
      SPDF_boundary = SpatialPolygons( w, proj4string=sp::CRS(proj4string0) )

      SPDF_boundary = gBuffer( gUnaryUnion( gBuffer( SPDF_boundary, width=spbuffer, byid=TRUE) ), width=spbuffer)
      # plot(SPDF_boundary)
    }

    SPI = gIntersection(  SPDF_boundary, SP, byid=TRUE ) # crop
    row.names(SPI) = row.names(SP)

    O = SpatialPolygonsDataFrame(SPI, data=data.frame( SPDF ), match="uid_internal" )
    row.names(O) = rn0
    O = sp::spChFIDs( O, rn0 )  #fix id's

    O$uid_internal = NULL

    O = spTransform( O, sp::CRS(proj4string0) )  # revert to input coordinate system


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

}
