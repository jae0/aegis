

aegis_mesh = function( pts, boundary="non_convex_hull", spbuffer=0, resolution=100, output_type="polygons", hull_multiplier=6, fraction_cv=1.0, fraction_good_bad=0.9, nAU_min=5, areal_units_constraint_nmin=1, tus=NULL ) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data
  #require(sp)
  #require(rgeos)
  require(sf)

  if (0) {
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")
    pts = as(meuse, "sf")
    boundary="non_convex_hull"
    spbuffer=NULL
    resolution=100
    output_type="polygons"
    hull_multiplier=6
    fraction_reduceby=0.01  # fraction of candidates to drop
    areal_units_constraint_nmin=1
    nAU_min=30

    res = aegis_mesh( pts=meuse ) # 0 snap buffer
    res = aegis_mesh( pts=meuse, spbuffer=50 ) # 50m snap buffer
    res = aegis_mesh( pts=meuse, resolution=1, spbuffer=50, output_type="grid" )
    res = aegis_mesh( pts=meuse, resolution=1, output_type="grid.count" )
    res = aegis_mesh( pts=meuse, resolution=1, spbuffer=50 )
    res = aegis_mesh( pts=meuse, resolution=5, spbuffer=50, areal_units_constraint_nmin=1 )

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)

    spplot( res, "zinc", col.regions=mypalette )

  }

  pts_crs = st_crs( pts )

  message( "'aegis_mesh' expects the projection to be in planar coordinates and also the same units as the resolution.")

  if (is.na(pts_crs)) stop("Input data does not have a projection?")


  if ( output_type=="grid" ) {
    require(raster)
    raster_template = raster(extent(pts))
    res(raster_template) = resolution
    crs(raster_template) = crs(pts_crs$proj4string) # projection(pts) # transfer the coordinate system to the raster
    rast = rasterize( as(pts, "Spatial"), raster_template, setdiff(names(pts), "geometry"), fun=mean, na.rm=TRUE) # not meaningful fir factors
    O = as( as(rast, "SpatialPolygonsDataFrame"), "sf")
    return(O)
  }


  if ( output_type=="grid.count" ) {
    require(raster)
    raster_template = raster(extent(pts)) # +1 to increase the area
    res(raster_template) = resolution
    crs(raster_template) = crs(pts_crs$proj4string) # projection(pts) # transfer the coordinate system to the raster
    pts$count = 1
    rast = rasterize( as(pts, "Spatial"), raster_template, field="count", fun="count", background=0) # not meaningful fir factors
    O = as( as( as(rast, "SpatialGridDataFrame"), "SpatialPointsDataFrame"), "sf")
    O = O[ O$layer > 0, ]
    return(O)
  }



  if ( output_type=="polygons" ) {

    # fine grid representation
    M = aegis_mesh( pts=pts,  resolution=resolution, output_type="grid.count" )
    xy = st_coordinates( M )

    if ( is.character(boundary) ) {
#      bnd = aegis_envelope( xy=xy, method=boundary, spbuffer=spbuffer, proj4string=pts_crs,  hull_multiplier=hull_multiplier )
      bnd = aegis_envelope( xy=xy, method=boundary, spbuffer=spbuffer,  hull_multiplier=hull_multiplier )
    } else {
      bnd = st_buffer( boundary, spbuffer )
    }
    st_crs(bnd) = pts_crs

    good = 1:nrow(M)
    nAU =  length(good) + 100  # offsets to start
    ntr = length(good) + 100

    if (!is.null(tus)) tuid = st_drop_geometry(pts) [, tus]


    message( "number of total areal units / number of candidate locations from which to drop " )

    finished = FALSE
    while(!finished) {
      AU = tessellate( xy[good,], outformat="sf", crs=pts_crs) # centroids via voronoi
      AU = st_sf( st_intersection( AU, bnd ) ) # crop
      AU$auid = 1:nrow(AU)
      AU_previous = AU
      vv = AU$auid[ unlist(st_intersects(pts, AU))] # index of matching AU
        if (is.null(tus)) {
          ww = tapply( rep(1, length(vv)), vv, sum, na.rm=T )
        } else {
          if ( length(vv) == length(tuid) ) {
            xx = xtabs(  ~ vv + tuid, na.action=na.omit )
            xx[xx > 0] = 1
            ww = rowSums(xx) # number of unique time units in each areal unit
          } else {
            break()
          }
        }
        AU$ww = ww[ match( as.character(AU$auid), names(ww) ) ]
        # AU$sa = st_area(AU) # [ match( names(ww), as.character(AU$auid) )]
        # AU$nden = AU$ww / AU$sa
        toremove = which( AU$ww < areal_units_constraint_nmin )
        ntr_previous = ntr
        ntr = length(toremove)
        ntr_delta = ntr_previous - ntr
        if (ntr > 1) {
          omin = min( unique( AU$ww[toremove] ))
          toremove_min = which( AU$ww == omin)
          if (length(toremove_min) > 0)  good =  good[-toremove_min]   #remove up to x% at a time
        }
        # check for convergence
        nAU_previous = nAU
        nAU = length(good)
        ntmean = mean( AU$ww, na.rm=TRUE)
        ntsd = sd( AU$ww, na.rm=TRUE)
        if (  (  ntsd/ntmean ) < fraction_cv ) if ( areal_units_constraint_nmin < ntmean  ) finished=TRUE   # when var is more constrained and mean is greater than target
        if ( (nAU-ntr) / nAU > fraction_good_bad ) finished=TRUE
        if ( ntr <= 1 ) finished =TRUE
        if ( ntr_delta <= 1  ) finished = TRUE
        if ( nAU == nAU_previous ) finished =TRUE
        if ( nAU <= nAU_min ) finished=TRUE
        message( nAU, "/ ", ntr  )
        # plot(AU[,"ww"])
        # (finished)
        # print (AU$ww[toremove] )
        # print( good)
    }

    AU = tessellate(xy[good,], outformat="sf", crs=pts_crs) # centroids via voronoi
    AU = st_intersection( AU, bnd ) # crop
    message( "Total number of areal units:  ", length(AU) )
    # plot(AU)
    return(AU)
  }

    if (0) {
      # make sure it is OK
      plot( coordinates(pts)[,2] ~ coordinates(O)[,2] )
      plot( coordinates(pts)[,1] ~ coordinates(O)[,1] )
      plot( pts$zinc ~ O$zinc )

      mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
      # mypalette = rev( heat.colors( 150 ) )
      #mypalette <- brewer.pal(8, "YlOrRd")

      pts$logz = log( pts$zinc )
      O$logz = log( O$zinc )

      spplot( pts, "zinc", col.regions=mypalette )
      spplot( O, "zinc", col.regions=mypalette )
    }

}
