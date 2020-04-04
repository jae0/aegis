

aegis_mesh = function( SPDF, SPDF_boundary="non_convex_hull", spbuffer=NULL, resolution=100, output_type="polygons", hull_multiplier=6, areal_units_tessilation_nmin=0, nreduceby=1 ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data

  require(sp)
  require(rgeos)


  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")

    res = aegis_mesh( SPDF=meuse) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, resolution=1, spbuffer=50, output_type="grid" )
    res = aegis_mesh( SPDF=meuse, resolution=1, output_type="grid.count" )
    res = aegis_mesh( SPDF=meuse, resolution=1, spbuffer=50 )
    res = aegis_mesh( SPDF=meuse, resolution=1, spbuffer=50, areal_units_tessilation_nmin=1 )

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)

    spplot( res, "zinc", col.regions=mypalette )

    SPDF_boundary="non_convex_hull"
    spbuffer=NULL
    resolution=100
    output_type="polygons"
    hull_multiplier=6
    areal_units_tessilation_nmin=0
    nreduceby=1

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
    O = as(O, "SpatialPointsDataFrame")
    O = O[ O$layer > 0, ]
    return(O)
  }



  if ( output_type=="polygons" ) {

    # fine grid representation
    SPDF$uuuid = 1:nrow(SPDF)  # internal id
    M = aegis_mesh( SPDF=SPDF,  resolution=resolution, output_type="grid.count" )

    rn0 = row.names(M)  # store for end
    M$uid_internal = rn0

    xy = try( coordinates(M) )
    if ( ("try-error" %in% class(xy)) ) stop( "Coordinates were not specified in data object?")
    rownames(xy) = M$uid_internal


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


    # define boundary of points if no boundary -- could also use convex hull ...
    if (SPDF_boundary=="gBuffer") {
      bnd = gBuffer( gUnaryUnion( gBuffer( M, width=spbuffer, byid=TRUE) ), width=spbuffer)
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
    SP = as(SPDF, "SpatialPoints" )

    for ( nmin in seq( from=0, to=areal_units_tessilation_nmin, by=nreduceby ) ) {
      SP = tessellate(coordinates( SP )) # centroids via voronoi
      sp::proj4string( SP ) = proj4string0
      SP = gIntersection(  bnd, SP, byid=TRUE ) # crop
      vv = over( SPDF, SP  )
      ww = tapply( rep(1, length(vv)), vv, sum, na.rm=T )
      good = which(ww >= nmin )
      ngood = length(good)
      if (ngood > 0 ) {
        xx = over( SP, SPDF[good,] )
        yy = which(is.finite( xx$uuuid ))
        SP = SP[ yy, ]
      }
      SP = gIntersection(  bnd, SP, byid=TRUE, checkValidity=2L ) # crop
    }

    SP = tessellate(coordinates( SP )) # centroids via voronoi
    sp::proj4string( SP ) = proj4string0
    SP = gIntersection(  bnd, SP, byid=TRUE ) # crop
    return(SP)

  }

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

}
