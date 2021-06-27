
geo_subset = function( spatial_domain, Z, method="sf" ) {

  #\\ filter locations based upon depths and location of spatial_domain
  #\\ most are in planar coords (so require plon, plat and z {depth} )

  # trim to extents
  ps = spatial_parameters( spatial_domain=spatial_domain ) # obtain internal projection params
  corners =  rbind(
    c( ps$corners$lon[1], ps$corners$lat[2] ),
    c( ps$corners$lon[2], ps$corners$lat[2] ),
    c( ps$corners$lon[2], ps$corners$lat[1] ),
    c( ps$corners$lon[1], ps$corners$lat[1] ),
    c( ps$corners$lon[1], ps$corners$lat[2] )
  )

  if ( spatial_domain %in% c( "canada.east.superhighres", "canada.east.highres", "canada.east" ) ) {
    z_range = c( 0, 5000 )
  }

  if ( spatial_domain == "SSE.mpa" ) {
    z_range = c( 0, 2000 )
  }

  if ( spatial_domain =="SSE" ) {
    z_range = c( 0, 800 )
  }

  if ( spatial_domain == "snowcrab" ) {
    z_range = c( 10, 350 )
  }

 
  if (method=="sf") {

    bnd = (
      st_multipoint( corners )
      %>% st_sfc( crs=st_crs(projection_proj4string("lonlat_wgs84")) )
      %>% st_cast("POLYGON" )
      %>% st_make_valid()
    )


    if (!inherits(Z, "sf")) {
      if (exists("lon", Z)) {
        Z = sf::st_as_sf( Z, coords = c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84"))  )
      } else if (exists("plon", Z)) {
        Z = sf::st_as_sf( Z, coords = c("plon","plat"), crs=st_crs(ps$aegis_proj4string_planar_km) )
      } else {
        Z = sf::st_as_sf( Z, coords = c(1,2), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
      }
    }

    bnd = st_transform(bnd, crs=st_crs(Z) ) 
 
    if (exists("z", Z, )) {
      inside = which( st_points_in_polygons( Z, bnd) & Z$z < z_range[2] & Z$z > z_range[1] )
    } else {
      inside = which( st_points_in_polygons( Z, bnd)  )
    }

    if ( spatial_domain == "SSE.mpa" ) {
      bnd = polygon_db( polyid="scotia.fundy.with.buffer" )
      bnd = st_transform(bnd, crs=st_crs(Z) ) 
      jj = which( st_points_in_polygons( Z[inside,], bnd)  )
      if (length( jj) > 0) inside = inside[ jj]
    }

    if ( spatial_domain == "snowcrab" ) {
      #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset
      Zco = as.data.frame( st_coordinates( st_transform(Z[inside,], st_crs(ps$aegis_proj4string_planar_km )) ) )
      colnames(Zco ) =c("plon", "plat")
      tokeep = polygon_inside( Zco[,c(1:2)], region="cfaall", planar=TRUE, proj.type=ps$aegis_proj4string_planar_km )

      # filter out area 4X
      corners = as.data.frame( cbind(
        lon = c(-63, -65.5, -56.8, -66.3 , -63),
        lat = c( 44.75, 43.8, 47.5, 42.8, 44.75 )
      ) )

      dd1 = which( Zco[,1] < corners$lon[1] & Zco[,2] > corners$lat[1]  )
      dd2 = which( Zco[,1] < corners$lon[2] & Zco[,2] > corners$lat[2]  )      
      dd3 = which( Zco[,1] > corners$lon[3] ) # east lim
      dd4 = which( Zco[,1] < corners$lon[4] )  #west lim
      dd5 = which( Zco[,2] > corners$lat[3]  ) # north lim
      dd6 = which( Zco[,2] < corners$lat[4]  )  #south lim
      
      todrop = unique( c(dd1, dd2, dd2, dd4, dd5, dd6) ) 

      if (length( tokeep) > 0 & length(todrop) > 0)  tokeep = setdiff( tokeep, todrop )

      if (length( tokeep) > 0)  inside = inside[ tokeep ]
    }

    return(inside)
  }


  if (method=="sp") {
    # faster ..
      if (!exists("plon", Z)) Z = lonlat2planar( Z, proj.type=ps$aegis_proj4string_planar_km ) # convert to internal projection
  
      inside = which(
        Z$plon >= ps$corners$plon[1] & Z$plon <= ps$corners$plon[2] &
        Z$plat >= ps$corners$plat[1] & Z$plat <= ps$corners$plat[2]  
      )

      if (exists("z", Z, )) {
        i = Z$z[inside] < z_range[2] & Z$z[inside] > z_range[1] 
        if (length(i) > 0) inside = inside[i]
      }
      
      if ( spatial_domain == "SSE.mpa" ) {
        bnd = polygon_db( polyid="scotia.fundy.with.buffer" )
        bnd = lonlat2planar( bnd, proj.type=ps$aegis_proj4string_planar_km ) # convert to internal projection
        jj = which( sp::point.in.polygon( Z$plon[inside], Z$plat[inside], bnd$plon, bnd$plat) != 0 )
        if (length( jj) > 0) inside = inside[ jj ]
      }

      if ( spatial_domain == "snowcrab" ) {
        #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset
        jj = polygon_inside( Z[ inside,c(1:2) ], region="cfaall", planar=TRUE, proj.type=ps$aegis_proj4string_planar_km )
        if (length( jj) > 0) inside = inside[ jj ]

        # filter out area 4X
        corners = data.frame( cbind(
          lon = c(-63, -65.5, -56.8, -66.3 ),
          lat = c( 44.75, 43.8, 47.5, 42.8 )
        ) )
        corners = lonlat2planar( corners, proj.type=ps$aegis_proj4string_planar_km )
        
        dd1 = which( Z$plon[inside] < corners$plon[1] & Z$plat[inside] > corners$plat[1]  )
        if (length( dd1) > 0) inside = inside[- dd1 ]

        dd2 = which( Z$plon[inside] < corners$plon[2] & Z$plat[inside] > corners$plat[2]  )
        if (length( dd2) > 0) inside = inside[- dd2 ]
        
        dd3 = which( Z$plon[inside] > corners$plon[3] ) # east lim
        if (length( dd3) > 0) inside = inside[- dd3 ]
        
        dd4 = which( Z$plon[inside] < corners$plon[4] )  #west lim
        if (length( dd4) > 0) inside = inside[- dd4 ]
        
        dd5 = which( Z$plat[inside] > corners$plat[3]  ) # north lim
        if (length( dd5) > 0) inside = inside[- dd5 ]
        
        dd6 = which( Z$plat[inside] < corners$plat[4]  )  #south lim
        if (length( dd6) > 0) inside = inside[- dd6 ]

      }
     return(inside)
 
  }

}


