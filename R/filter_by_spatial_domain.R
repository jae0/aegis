filter_by_spatial_domain = function(Z, spatial_domain ) {
  
  inside = 1:nrow(Z)

  if (exists("z", Z )) {
    z_range = switch( spatial_domain,
      canada.east.superhighres = c( 0, 5000 ),
      canada.east.highres = c( 0, 5000 ),
      canada.east = c( 0, 5000 ),
      SSE.mpa = c( 0, 2000 ),
      SSE = c( 0, 800 ), 
      snowcrab = c( 10, 350 )
    )
    inside = inside[ which( Z$z < z_range[2] & Z$z > z_range[1] ) ]
  }  


  # trim to extents
  ps = spatial_parameters( spatial_domain=spatial_domain ) # obtain internal projection params


  if (inherits( Z, "sf")) {

    domain_extent =  rbind(
      c( ps$corners$lon[1], ps$corners$lat[2] ),
      c( ps$corners$lon[2], ps$corners$lat[2] ),
      c( ps$corners$lon[2], ps$corners$lat[1] ),
      c( ps$corners$lon[1], ps$corners$lat[1] ),
      c( ps$corners$lon[1], ps$corners$lat[2] )
    )

    bnd = sf_pts2poly(domain_extent)
    bnd = st_transform(bnd, crs=st_crs(Z) ) 
    i = which( st_points_in_polygons( Z, bnd, method="sp::point.in.polygon")  )  # fastest .. can use "sf" only as well with method="sf_fast" ..
    if (length(i) > 0) inside = inside[i]
    
    if ( spatial_domain == "SSE.mpa" ) {
      bnd = polygon_db( polyid="scotia.fundy.with.buffer" )
      bnd = st_transform(bnd, crs=st_crs(Z) ) 
      i = which( st_points_in_polygons( Z[inside,], bnd, method="sp::point.in.polygon" )  )
      if (length( i) > 0) inside = inside[ i]
    }

    if ( spatial_domain == "snowcrab" ) {
      #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset
      y = data.table::fread( aegis.polygons::polygon_file("cfaall" ) )
      names(y) =c("lon", "lat")
      bnd = sf_pts2poly(y)
      bnd = st_make_valid(  bnd  )
      bnd = st_transform(bnd, crs=st_crs(Z) ) 

      i = which( st_points_in_polygons( Z[inside,], bnd, method="sp::point.in.polygon" ) )
      if (length( i) > 0) inside = inside[ i]


      # filter out area 4X
      fix_4x = as.data.frame( cbind(
        lon = c(-63,   -65.5,  -56.8, -66.3 , -63    ),
        lat = c( 44.75, 43.8,   47.5,  42.8,   44.75 )
      ) )

      fix_4x = st_multipoint( as.matrix(fix_4x) )
      fix_4x = st_sfc( fix_4x, crs=st_crs(projection_proj4string("lonlat_wgs84")) )
      fix_4x = st_transform( fix_4x, crs=st_crs(Z) )
      fix_4x = st_coordinates(fix_4x)

      Zco = st_coordinates( Z[inside,] )  

      dd1 = which( Zco[,1] < fix_4x[1,1] & Zco[,2] > fix_4x[1,2]  )
      dd2 = which( Zco[,1] < fix_4x[2,1] & Zco[,2] > fix_4x[2,2]  )      
      dd3 = which( Zco[,1] > fix_4x[3,1] ) # east lim
      dd4 = which( Zco[,1] < fix_4x[4,1] )  #west lim
      dd5 = which( Zco[,2] > fix_4x[3,2]  ) # north lim
      dd6 = which( Zco[,2] < fix_4x[4,2]  )  #south lim
      
      todrop = unique( c(dd1, dd2, dd2, dd4, dd5, dd6) ) 

      if (length( inside) > 0 & length(todrop) > 0)  inside = inside[ - todrop ]
    
    }
        
    return(inside)

  } else if (inherits( Z, "Spatial")) {
         
    return(stop("This method not yet implenented" ))

  } else {

    # faster .. direct
 
      if (exists("lon", Z)) {
        Z = lonlat2planar( Z, proj.type=ps$aegis_proj4string_planar_km ) # convert to internal projection
      }

      i = which(
        Z$plon[inside] >= ps$corners$plon[1] & Z$plon[inside] <= ps$corners$plon[2] &
        Z$plat[inside] >= ps$corners$plat[1] & Z$plat[inside] <= ps$corners$plat[2]  
      )
      if (length(i) > 0) inside = inside[i]
       
      if ( spatial_domain == "SSE.mpa" ) {
        bnd = polygon_db( polyid="scotia.fundy.with.buffer" )
        bnd = data.table( st_coordinates(bnd) )
        names(bnd ) = c("lon", "lat", "l1", "l2")

        bnd = lonlat2planar( bnd, proj.type=ps$aegis_proj4string_planar_km ) # convert to internal projection
        i = which( sp::point.in.polygon( Z$plon[inside], Z$plat[inside], bnd$plon, bnd$plat) != 0 )
        if (length( i) > 0) inside = inside[ i ]
      }

      if ( spatial_domain == "snowcrab" ) {

        # filter out area 4X
        fix_4x = data.frame( cbind(
          lon = c(-63, -65.5, -56.8, -66.3 ),
          lat = c( 44.75, 43.8, 47.5, 42.8 )
        ) )
        fix_4x = lonlat2planar( fix_4x, proj.type=ps$aegis_proj4string_planar_km )
        
        dd1 = which( Z$plon[inside] < fix_4x$plon[1] & Z$plat[inside] > fix_4x$plat[1]  )
        if (length( dd1) > 0) inside = inside[- dd1 ]

        dd2 = which( Z$plon[inside] < fix_4x$plon[2] & Z$plat[inside] > fix_4x$plat[2]  )
        if (length( dd2) > 0) inside = inside[- dd2 ]
        
        dd3 = which( Z$plon[inside] > fix_4x$plon[3] ) # east lim
        if (length( dd3) > 0) inside = inside[- dd3 ]
        
        dd4 = which( Z$plon[inside] < fix_4x$plon[4] )  #west lim
        if (length( dd4) > 0) inside = inside[- dd4 ]
        
        dd5 = which( Z$plat[inside] > fix_4x$plat[3]  ) # north lim
        if (length( dd5) > 0) inside = inside[- dd5 ]
        
        dd6 = which( Z$plat[inside] < fix_4x$plat[4]  )  #south lim
        if (length( dd6) > 0) inside = inside[- dd6 ]

        #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset
        i = polygon_inside( Z[ inside, c(1:2) ], region="cfaall", planar=TRUE, proj.type=ps$aegis_proj4string_planar_km )

        if (length(i) > 0) inside = inside[i]
       
      }
      
      return(inside)
  
  }

  return( stop("Data not recognized" ) )
}
