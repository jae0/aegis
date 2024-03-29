

aegis_mesh = function( pts, boundary=NULL, spbuffer=0, resolution=100, output_type="polygons",  
  hull_lengthscale=NULL, fraction_cv=1.0, fraction_todrop=1/10, nAU_min=5, count_time=FALSE,
  areal_units_constraint_ntarget=1, areal_units_constraint_nmin=1, tus="none", verbose=FALSE, using_density_based_removal=FALSE 
) {

# fraction_cv = 1 means  ~ poisson

  # wrapper to tessellate (tile geometry), taking spatial points data and converting to spatial polygons data
 
  require(sf)
  require(stars)
  
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
    hull_lengthscale=50
    areal_units_constraint_ntarget=1
    nAU_min=30
    fraction_todrop=1/10
    fraction_cv=1.0
    tus="none"

    res = aegis_mesh( pts=pts, hull_lengthscale=50 ) # 0 snap buffer
    res = aegis_mesh( pts=pts, hull_lengthscale=50, spbuffer=25 ) # 50m snap buffer
    res = aegis_mesh( pts=pts, hull_lengthscale=25, resolution=1, spbuffer=25, output_type="grid" )
    res = aegis_mesh( pts=pts, hull_lengthscale=25, resolution=1, output_type="grid.count" )
    res = aegis_mesh( pts=pts, hull_lengthscale=25, resolution=1, spbuffer=25 )
    res = aegis_mesh( pts=pts, hull_lengthscale=25, resolution=5, spbuffer=25, areal_units_constraint_ntarget=1 )

    mypalette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)

    spplot( res, "zinc", col.regions=mypalette )

  }

  pts_crs = st_crs( pts )

  # message( "'aegis_mesh' expects the projection to be in planar coordinates and also the same units as the resolution.")

  if (is.na(pts_crs)) stop("Input data does not have a projection?")


  if ( output_type=="grid" ) {
    rast = stars::st_rasterize( pts, dx=resolution, dy=resolution )
    O = stars::st_apply(rast, c("x", "y"), mean, na.rm=TRUE)
    # O = sf::st_as_sf( rast, as_points=TRUE, na.rm=FALSE )
    # st_crs(O) = st_crs( pts )
    return(O)
  }


  if ( output_type=="grid.count" ) {
    pts$count = 1
    rast = stars::st_rasterize( pts["count"], dx=resolution, dy=resolution )
    O = stars::st_apply(rast, c("x", "y"), sum, na.rm=TRUE)
    O = sf::st_as_sf( rast, as_points=TRUE, na.rm=FALSE )
    st_crs(O) = st_crs( pts )
    return(O)
  }



  if ( output_type=="polygons" ) {

    # fine grid representation
    M = aegis_mesh( pts=pts,  resolution=resolution, output_type="grid.count" )
    M = M[ which(M$count > 0), ]
    M_xy = st_coordinates( M ) 

    if ( is.null(boundary)) boundary="non_convex_hull"
    if ( is.character(boundary) ) {
      bnd = aegis_envelope( xy=M_xy, method=boundary, spbuffer=spbuffer,  hull_lengthscale=hull_lengthscale )
      st_crs(bnd) = pts_crs
    } else {
      bnd = st_transform(boundary, pts_crs)
    }
    bnd = st_buffer( bnd, spbuffer )

    if (0) {
      plot(bnd, reset=FALSE)
      plot(M, add=TRUE)
    }

    M_tokeep = 1:nrow(M)
    nAU = length(M_tokeep) + 100  # offsets to start
    ntr = length(M_tokeep) + 100

    probs = c(fraction_todrop/2, 1-(fraction_todrop/2))

    if (tus !="none") {
      oo = st_drop_geometry(pts)
      oo = as.data.frame(oo)
      time_id = oo [, tus]
      oo = NULL
    }

    message( "Targets:" )
    message( "areal_units_constraint_ntarget: ", areal_units_constraint_ntarget )
    message( "fraction_cv: ", fraction_cv )

    finished = FALSE
    while(!finished) {

      AU = tessellate( M_xy[M_tokeep,], outformat="sf", crs=pts_crs) # centroids via voronoi
      AU = st_as_sf(AU)
   #   AU = st_sf( st_intersection( AU, bnd ) ) # crop

      if(0) {
        x11();
        plot(bnd, reset=FALSE)
        plot(AU, add=T)
        plot(M, add=T)
      }

      AU$auindex = 1:nrow(AU)
      pts_auindex = st_points_in_polygons( pts, AU, varname="auindex" )
      AU$npts  = 0
      np = length(pts_auindex)
      if ( tus == "none" ) {
          m = data.table(pts_auindex=pts_auindex)
          m = na.omit(m)
          m = m[,.(npts=.N), by=.(pts_auindex) ]  # number of unique time units in each areal unit
      } else {
        if ( np == length(time_id) ) {
          m = as.data.table(cbind(pts_auindex, time_id))
          m = na.omit(m)
          m = m[, .(npts=.N), by=.(pts_auindex, time_id)]
          if (count_time)  m$npts[ m$npts > 0] = 1
          m = m[,.(npts=sum(npts)), by=.(pts_auindex) ]  # number of unique time units in each areal unit
        } else {
          break()
        }
      }
      
      kk = match( m$pts_auindex, as.character(AU$auindex))
      kkx = (which(!is.finite(kk)))
      if (length(kkx) > 0) {
        m = m[-kkx, ]
        kk = kk[-kkx]
      }

      AU$npts[ kk ] = m$npts
      AU$npts[ which(!is.finite(AU$npts)) ] = 0
      
      if (using_density_based_removal) {
        # testing density based removal
        AU$sa = st_area(AU) # [ match( names(npts), as.character(AU$auindex) )]
        AU$density = AU$npts / AU$sa
      }

      au_toremove = au_toremove_candidates = NULL
      au_toremove_candidates = which( AU$npts < areal_units_constraint_ntarget )
      
      ntr_previous = ntr
      ntr = length(au_toremove_candidates)
      ntr_delta = ntr_previous - ntr
      
      if (ntr > 1) {
        # force removal criterion: smallest counts 
        ucounts = sort( unique( AU$npts ) )
        if (ucounts[1] == 0) au_toremove  = unique( c(au_toremove, which( AU$npts <= areal_units_constraint_nmin ),  which(AU$npts == min(ucounts) ) ) )

        drop_threshold = quantile( AU$npts, probs=fraction_todrop, na.rm=TRUE )
        au_toremove_candidates = unique(c( 
          au_toremove_candidates,
          which( AU$npts <= drop_threshold )
        ))
        if (using_density_based_removal) {
          dd = stats::quantile( AU$density, probs=probs, na.rm=TRUE )
          ss = stats::quantile( AU$sa, probs=probs, na.rm=TRUE )
          au_toremove_candidates = intersect(
            au_toremove_candidates, 
            which( ( AU$density < dd[1] ) | ( AU$sa < ss[1] ) )
          ) 
        }

        rv = length(au_toremove_candidates)
        if (rv > 5) {
          au_toremove_candidates = au_toremove_candidates[ sample( rv, max(1, floor(rv*fraction_todrop) )) ]
        }
        au_toremove = na.omit( unique(c(au_toremove, au_toremove_candidates  ) ) )
        M_tokeep = M_tokeep[- au_toremove ]  # update M_tokeep list 
      }
      
      # check for convergence
      nAU_previous = nAU
      nAU = length(M_tokeep)
      ntmean = mean( AU$npts, na.rm=TRUE )
      ntsd = sd( AU$npts, na.rm=TRUE )

      if (verbose) message( "nAU: ", nAU, " ;   mean no pts: ", round(ntmean,2), " ;  sd no pts: ", round(ntsd,2), " ;  sd/mean no pts: ", round(ntsd/ntmean, 2) )

      if ( ntmean > areal_units_constraint_ntarget   ) {
        if (verbose) message ("breaking on criterion: areal_units_constraint_ntarget")
        finished=TRUE   # when var is more constrained and mean is greater than target
      }
      if ( (ntmean > areal_units_constraint_ntarget) & (  ntsd/ntmean ) <= fraction_cv ) {
        if (verbose) message ("breaking on criterion: areal_units_constraint_ntarget & fraction_cv")
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
      # print (AU$npts[au_toremove] )
      # print( M_tokeep)
    }

    AU = tessellate(M_xy[M_tokeep,], outformat="sf", crs=pts_crs) # centroids via voronoi
    AU = st_sf( st_intersection( AU, bnd ) ) # crop
    message( "After tesselation, there are:  ", nrow(AU), " areal units." )

    if (verbose)  plot( AU )

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
