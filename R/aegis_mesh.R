

aegis_mesh = function( SPDF, SPDF_boundary="non_convex_hull", spbuffer=NULL, k=5, resolution=100, output_type="polygons", maxtries=10, hull_multiplier=6, spbuffer_factor=1.25 ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data

  require(sp)
  require(rgeos)


  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")

    res = aegis_mesh( SPDF=meuse) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50, output_type="grid" )
    res = aegis_mesh( SPDF=meuse, resolution=1, output_type="grid.count" )
    res = as( res, "SpatialPointsDataFrame")
    res = res[ res$layer > 0 ,]

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)

    spplot( res, "zinc", col.regions=mypalette )

  }

  message( "Warning: 'aegis_mesh' expects the projection to be in planar coordinates and also the same units as the resolution.")

  proj4string0 = sp::proj4string( SPDF )

  if (is.na(proj4string0)) stop("Input data does not have a projection")


  if ( output_type=="grid" ) {
    require(raster)
    raster_template = raster(extent(sp::bbox(SPDF))) # +1 to increase the area
    res(raster_template) = resolution
    crs(raster_template) = sp::CRS(proj4string0) # projection(SPDF) # transfer the coordinate system to the raster
    rast = rasterize(SPDF, raster_template, names(SPDF), fun=mean, na.rm=TRUE) # not meaningful fir factors
    O = as(rast, "SpatialGridDataFrame")
    return(O)
  }


  if ( output_type=="grid.count" ) {
    require(fasterize)
    require(raster)
    raster_template = raster(extent(sp::bbox(SPDF))) # +1 to increase the area
    res(raster_template) = resolution
    crs(raster_template) = sp::CRS(proj4string0) # projection(SPDF) # transfer the coordinate system to the raster
    SPDF$count = 1
    rast = rasterize( SPDF, raster_template, field="count", fun="count", background=0) # not meaningful fir factors
    O = as(rast, "SpatialGridDataFrame")
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

    drange = range( c( diff(range( xy[,1] )), diff(range(xy[,2] )) ) )
    spbuffer_default =  floor( min(drange)/ 25 )

    if (is.null(spbuffer)) {
      spbuffer = spbuffer_default
      message( "spbuffer not set, using spbuffer=", spbuffer)
    }

    if (spbuffer < spbuffer_default / 4 ) {
      spbuffer = spbuffer_default
      message( "spbuffer very low. Setting initial spbuffer =", spbuffer/4)
    }

    ntries = 0

    finished = FALSE
    while ( !finished ) {

      if (ntries > 0) {
        spbuffer = round( spbuffer * spbuffer_factor, 3 )
        message( "Insufficient spbuffer to get all areal units, increasing spbuffer to: ", spbuffer )
      }

      # define boundary of points if no boundary -- could also use convex hull ...
      if (SPDF_boundary=="gBuffer") {
        bnd = gBuffer( gUnaryUnion( gBuffer( SPDF, width=spbuffer, byid=TRUE) ), width=spbuffer)
        # plot(bnd)
      }

      if (SPDF_boundary=="concave.hull") {
        v = concave.hull( xy, ub=spbuffer*hull_multiplier)
        if ( any( !is.finite(v) )) next()
        w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
        bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string0) )
        bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
        # plot(bnd)
      }

      if (SPDF_boundary=="non_convex_hull") {
        v = non_convex_hull( xy, alpha=spbuffer*hull_multiplier  )
        if ( any( !is.finite(v) )) next()
        w = list( Polygons(list( Polygon( as.matrix( v ) )), ID="boundary" ))
        bnd = SpatialPolygons( w, proj4string=sp::CRS(proj4string0) )
        bnd = gBuffer( gUnaryUnion( gBuffer( bnd, width=spbuffer, byid=TRUE) ), width=spbuffer)
          # plot(bnd)
      }

      sp::proj4string( bnd ) = proj4string0
      sp::proj4string( SP ) = proj4string0

      SPI = gIntersection(  bnd, SP, byid=TRUE ) # crop
      if (length(SPI) == length(SP) ) finished = TRUE

      ntries = ntries + 1
      if (ntries >= maxtries) finished = TRUE
    }

    if (length(SPI) != length(SP) ) {
      stop( "The size of the spbuffer is too small to recover all the areal units from a Voroni tessilation, increase the distance..." )
    }
    row.names(SPI) = row.names(SP)

    O = SpatialPolygonsDataFrame(SPI, data=data.frame( SPDF ), match="uid_internal" )
    row.names(O) = rn0
    attr( O, "spbuffer") = spbuffer
    message( "Final spbuffer = ", spbuffer )
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