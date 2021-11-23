

aegis_mesh = function( pts, boundary=NULL, spbuffer=0, resolution=100, output_type="polygons",  
  hull_alpha=15, fraction_cv=1.0, fraction_todrop=1/10, nAU_min=5, 
  areal_units_constraint_ntarget=1, tus="none", verbose=FALSE, using_density_based_removal=FALSE 
) {

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data
  #require(rgeos)
  require(sf)

  if (0) {
    require(sp)
    data(meuse)
    coordinates(meuse) = ~ x+y
    sp::proj4string(meuse) = CRS("+init=epsg:28992")
    pts = as(meuse, "sf")
    boundary=NULL
    spbuffer=25
    resolution=100
    output_type="polygons"
    hull_alpha=50
    areal_units_constraint_ntarget=1
    nAU_min=30
    fraction_todrop=1/10
    fraction_cv=1.0
    tus="none"

    res = aegis_mesh( pts=pts, hull_alpha=50 ) # 0 snap buffer
    res = aegis_mesh( pts=pts, hull_alpha=50, spbuffer=25 ) # 50m snap buffer
    res = aegis_mesh( pts=pts, hull_alpha=25, resolution=1, spbuffer=25, output_type="grid" )
    res = aegis_mesh( pts=pts, hull_alpha=25, resolution=1, output_type="grid.count" )
    res = aegis_mesh( pts=pts, hull_alpha=25, resolution=1, spbuffer=25 )
    res = aegis_mesh( pts=pts, hull_alpha=25, resolution=5, spbuffer=25, areal_units_constraint_ntarget=1 )

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)

    spplot( res, "zinc", col.regions=mypalette )

  }

  pts_crs = st_crs( pts )

  # message( "'aegis_mesh' expects the projection to be in planar coordinates and also the same units as the resolution.")

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
    return(O)
  }



  if ( output_type=="polygons" ) {

    # fine grid representation
    M = aegis_mesh( pts=pts,  resolution=resolution, output_type="grid.count" )
    M = M[ M$layer > 0, ]
    xy = st_coordinates( M ) 

    if ( is.null(boundary)) boundary="non_convex_hull"
    if ( is.character(boundary) ) {
      bnd = aegis_envelope( xy=xy, method=boundary, spbuffer=spbuffer,  hull_alpha=hull_alpha )
      st_crs(bnd) = pts_crs
    } else {
      bnd = st_transform(boundary, pts_crs)
    }
    bnd = st_buffer( bnd, spbuffer )

    if (0) {
      plot(bnd, reset=FALSE)
      plot(M, add=TRUE)
    }
    tokeep = 1:nrow(M)
    nAU =  length(tokeep) + 100  # offsets to start
    ntr = length(tokeep) + 100

    probs = c(fraction_todrop/2, 1-(fraction_todrop/2))

    if (tus !="none") time_id = st_drop_geometry(pts) [, tus]

    finished = FALSE
    while(!finished) {

      # st_voronoi inside of the tesselation handles duplicates poorly. catch them here and adjust
      oo = which(duplicated(xy[tokeep,]))
      if (length(oo) > 0) stop("Duplicated positions created. This should not happen.")

      AU = tessellate( xy[tokeep,], outformat="sf", crs=pts_crs) # centroids via voronoi
      AU = st_as_sf(AU)

      if(0) {
        x11();
        plot(bnd, reset=FALSE)
        plot(AU, add=T)
        plot(M, add=T)
      }
      AU = st_sf( st_intersection( AU, bnd ) ) # crop
      AU$auindex = 1:nrow(AU)
      pts_auindex = st_points_in_polygons( pts, AU, varname="auindex" )
      AU$npts  = 0
      np = length(pts_auindex)
      if ( tus == "none" ) {
        npts = tapply( 1:np, pts_auindex, length, default = 0) # == table(fac)
      } else {
        if ( np == length(time_id) ) {
          m = as.data.table(cbind(pts_auindex, time_id))
          m = na.omit(m)
          xx = xtabs(  ~  pts_auindex + time_id, m, na.action=na.omit )
          xx[xx > 0] = 1
          npts = rowSums(xx, na.rm=FALSE) # number of unique time units in each areal unit
        } else {
          break()
        }
      }
      AU$npts[ match( names(npts), as.character(AU$auindex)) ] = npts
      AU$npts[ which(!is.finite(AU$npts)) ] = 0
      if (using_density_based_removal) {
        # testing density based removal
        AU$sa = st_area(AU) # [ match( names(npts), as.character(AU$auindex) )]
        AU$density = AU$npts / AU$sa
      }

      toremove = NULL
      toremove = which( AU$npts < areal_units_constraint_ntarget )
      
      ntr_previous = ntr
      ntr = length(toremove)
      ntr_delta = ntr_previous - ntr
      
      if (ntr > 1) {
        # removal criterion: smallest counts 
        # toremove = unique( c( toremove, which(AU$npts == min(unique(AU$npts))) ) )
        drop_threshold = quantile( AU$npts, probs=fraction_todrop, na.rm=TRUE )
        toremove = unique( c( toremove, which( AU$npts <= drop_threshold ) ) )
        if (length(toremove) > 1) {
          if (using_density_based_removal) {
            dd = stats::quantile( AU$density, probs=probs, na.rm=TRUE )
            ss = stats::quantile( AU$sa, probs=probs, na.rm=TRUE )
            toremove = intersect(toremove, which( 
              ( AU$npts %in% omin ) & 
              ( ( AU$density < dd[1] ) | ( AU$sa < ss[1] ) )
            ) )
          } 
          if (length(toremove) > 0 )  {
            print( length(tokeep ))
            toremove = na.omit(toremove)
            ntr = length(toremove)
            if (ntr > 5){
              toremove = toremove[ sample( ntr, max(1, floor(ntr*fraction_todrop) )) ]
            }
            tokeep = tokeep[- toremove ]  # update tokeep list 
          }
        }
      }
      
      # check for convergence
      nAU_previous = nAU
      nAU = length(tokeep)
      ntmean = mean( AU$npts, na.rm=TRUE )
      ntsd = sd( AU$npts, na.rm=TRUE )

      if (verbose) message( "nAU: ", nAU, " ;   mean no pts: ", round(ntmean,2), " ;  sd no pts: ", round(ntsd,2), " ;  sd/mean no pts: ", round(ntsd/ntmean, 2) )

      if ( ntmean > areal_units_constraint_ntarget   ) {
        if (verbose) message ("breaking on criterion: areal_units_constraint_ntarget")
        finished=TRUE   # when var is more constrained and mean is greater than target
      }
      if (  (  ntsd/ntmean ) <= fraction_cv ) {
        if (verbose) message ("breaking on criterion: fraction_cv")
        finished=TRUE   # when var is more constrained and mean is greater than target
      }
       if ( ntr <= 1 ) {
        if (verbose) message ("breaking on criterion: no more removal candidates")
        finished=TRUE
      }
       if ( ntr_delta <= 1  ) {
         if (verbose) message ("breaking on criterion: incremental change in au's stable")
         finished=TRUE
       }
      if ( nAU == nAU_previous ) {
        if (verbose) message ("breaking on criterion: incremental change in au's stable")
        finished=TRUE
      }
      if ( nAU <= nAU_min ) {
        if (verbose) message ("breaking on criterion: removal candidates exceeded")
        finished=TRUE
      }
#      if (verbose) message( "Current number of total areal units: ", nAU )
      # plot(AU[,"npts"])
      # (finished)
      # print (AU$npts[toremove] )
      # print( tokeep)
    }

    AU = tessellate(xy[tokeep,], outformat="sf", crs=pts_crs) # centroids via voronoi
    AU = st_sf( st_intersection( AU, bnd ) ) # crop
    message( "After tesselation, there are:  ", nrow(AU), " areal units." )
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
