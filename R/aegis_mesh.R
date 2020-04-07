

aegis_mesh = function( SPDF, SPDF_boundary="non_convex_hull", spbuffer=NULL, resolution=100, output_type="polygons", hull_multiplier=6, nreduceby=3, nAU_min=30, areal_units_constraint_nmin=1 ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data
  require(sp)
  require(rgeos)

  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")
    SPDF = meuse
    SPDF_boundary="non_convex_hull"
    spbuffer=NULL
    resolution=100
    output_type="polygons"
    hull_multiplier=6
    nreduceby=3
    areal_units_constraint_nmin=1
    nAU_min=30


    res = aegis_mesh( SPDF=meuse) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, spbuffer=50 ) # 50m snap buffer
    res = aegis_mesh( SPDF=meuse, resolution=1, spbuffer=50, output_type="grid" )
    res = aegis_mesh( SPDF=meuse, resolution=1, output_type="grid.count" )
    res = aegis_mesh( SPDF=meuse, resolution=1, spbuffer=50 )
    res = aegis_mesh( SPDF=meuse, resolution=5, spbuffer=50, areal_units_constraint_nmin=1 )

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
    O = as(O, "SpatialPointsDataFrame")
    O = O[ O$layer > 0, ]
    return(O)
  }



  if ( output_type=="polygons" ) {

    # fine grid representation
    M = aegis_mesh( SPDF=SPDF,  resolution=resolution, output_type="grid.count" )
    xy = coordinates( M )
    bnd = aegis_envelope( xy=xy, method=SPDF_boundary, spbuffer=spbuffer, returntype="SpatialPolygons", proj4string=proj4string0, hull_multiplier=hull_multiplier )

    good = 1:nrow(M)
    nAU =  length(good)
    SP0 = as(SPDF, "SpatialPoints")
    M= NULL
    SPDF = NULL
    gc()

    finished = FALSE
    while(!finished) {
      AU = tessellate(xy[good,]) # centroids via voronoi
      sp::proj4string( AU ) = proj4string0
      AU = gIntersection(  bnd, AU, byid=TRUE ) # crop
      sa = gArea(AU, byid=TRUE)
      vv = over( SP0, AU  )
      ww = tapply( rep(1, length(vv)), vv, sum, na.rm=T )
      nden = ww / sa
      toremove = which(
        ww < areal_units_constraint_nmin &
        sa < median((sa)  &
        nden < median((nden)
      )
      ntr = length(toremove)
      if (ntr > 0 ) {
        toremove = toremove[ order(nden) ]
        good = setdiff( good, good[ toremove[ 1:min(nreduceby, ntr)]] )  #remove up to 3 a a time
      }
      nAU_previous = nAU
      nAU = length(good)
      if ( nAU <= nAU_min ) finished=TRUE
      if ( nAU == nAU_previous ) finished =TRUE

      plot(AU)
      print(nAU)
    }

    AU = tessellate(xy[good,]) # centroids via voronoi
    sp::proj4string( AU ) = proj4string0
    AU = gIntersection(  bnd, AU, byid=TRUE ) # crop

    # plot(AU)

    return(AU)

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
