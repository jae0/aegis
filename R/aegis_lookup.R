
aegis_lookup = function( 
  data_class="bathymetry", 
  variable_name=NULL,
  LOCS=NULL, 
  AU_target=NULL, 
  AU=NULL, 
  lookup_from="core", 
  lookup_to="points", 
  lookup_from_class="aggregated_data", 
  tz="America/Halifax", 
  year.assessment=NULL ,
  FUNC=mean
) {
 
 if (0) {
  #  M = ...
    z = aegis_lookup( data_class="temperature", LOCS=M[, c("lon", "lat")], spatial_domain=p$spatial_domain, lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data", variable_name="t.mean" ) # core=="rawdata"
 }

 stop ("fix DT issues with varianle_name")

  require(data.table)  # enforce

  crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))

  for (dc in data_class) {

    # determine LU (lookup table)
    LU = NULL

    if ( "bathymetry" %in% dc ) {

      p = bathymetry_parameters(  project_class=lookup_from  )
      if ( lookup_from %in% c("core" ) )  LU = bathymetry_db ( p=p, DS=lookup_from_class )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c("stmv", "hybrid") ) LU = bathymetry_db ( p=p, DS="complete" )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c("carstm" )) LU = carstm_model( p=p, DS="carstm_modelled_summary" ) 
    }

    if ( "substrate" %in% dc ) {
      p = substrate_parameters(  project_class=lookup_from  )
      if ( lookup_from %in% c("core" ) )  LU = substrate_db ( p=p, DS=lookup_from_class )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c("stmv", "hybrid") ) {
        LU = substrate_db ( p=p, DS="complete" )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
        pB = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class=lookup_from  )
        BA = bathymetry_db ( p=pB, DS="baseline", varnames=c("lon", "lat")  )
        LU = cbind(LU, BA )
      }
      if ( lookup_from %in% c("carstm" )) LU = carstm_model( p=p, DS="carstm_modelled_summary" ) 
    }

    if ( "temperature" %in% dc ) {
      if (is.null(year.assessment)) year.assessment = max( lubridate::year(LOCS$timestamp) )
      p = temperature_parameters(  project_class=lookup_from, year.assessment=year.assessment )
      if ( lookup_from %in% c("core" ) )  LU = temperature_db ( p=p, DS=lookup_from_class )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c("stmv", "hybrid") )  LU = temperature_db ( p=p, DS="complete" )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c("carstm" )) LU = carstm_model( p=p, DS="carstm_modelled_summary" ) 
    }

    if ("speciescomposition" %in% dc ){
      if (is.null(year.assessment)) year.assessment = max( lubridate::year(LOCS$timestamp) )
      p = speciescomposition_parameters(  project_class=lookup_from, variabletomodel=variable_name, year.assessment=year.assessment  )
      if ( lookup_from %in% c("core" ) ) LU = speciescomposition_db ( p=p, DS="speciescomposition" )   # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
      if ( lookup_from %in% c( "stmv", "hybrid") )  LU = aegis_db( p=p, DS="complete" )   
      if ( lookup_from %in% c("carstm" )) LU = carstm_model( p=p, DS="carstm_modelled_summary" ) 
    }

    if (is.null(LU)) stop( "lookup data not found")

    if (is.null(variable_name)) variable_name = setdiff( names(LU), c("plon", "plat", "lon", "lat"))
 
    # ------------------

    if (p$aegis_dimensionality =="space" ) {

      if ( lookup_from %in% c("core") & lookup_to == "points" )  {
        # matching to point (LU) to point (LOCS)
        # if any still missing then use stmv depths
        if (!exists("plon", LU)) LU = lonlat2planar(LU, proj.type=p$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        for (vnm in variable_name) {
          LOCS[[vnm]] = LU[ match(
              array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ),
              array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams )
          ), vnm ]
        }
        return( LOCS[[variable_name]] )
      }

      if ( lookup_from %in% c("core") & lookup_to == "areal_units" )  {
        # point (LU) -> areal unit (LOCS)
        if (!exists("lon", LU)) LU = planar2lonlat(LU, p$aegis_proj4string_planar_km)
        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(LOCS) )
        for (vnm in variable_name) {
          LOCS[, vnm] = aggregate( LU[, vnm], LOCS, FUNC, na.rm=TRUE ) [[vnm]]
        }
        return( st_drop_geometry(LOCS)[,variable_name] )
      }


      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {
        # matching to point (LU) to point (LOCS)
        if (!exists("plon", LU)) LU = lonlat2planar(LU, proj.type=p$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        for (vnm in variable_name) {
          LOCS[,vnm ] = LU[ match(
            array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ),
            array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams )
          ), vnm ]
        }
        return( LOCS[,variable_name] )
      }

      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
        # point (LU) -> areal unit (LOCS)
        if (!exists("lon", LU)) LU = planar2lonlat(LU, p$aegis_proj4string_planar_km)

        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(LOCS) )
        for (vnm in variable_name) {
          LOCS[, vnm] = aggregate( LU[, vnm], LOCS, FUNC, na.rm=TRUE ) [[vnm]]
        }
        return( st_drop_geometry(LOCS)[,variable_name] )
      }


      if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
        # areal unit (LU) to point (LOCS) --- expects 
        AU = areal_units( p=p )  #  poly associated with AU
        AU = sf::st_transform( AU, crs=st_crs(p$aegis_proj4string_planar_km) )
        bm = match( AU$AUID, LU$space )
        AU[[variable_name]] = LU[["predictions"]][ bm, "mean" ]
        LU = AU

        if (!exists("lon", LOCS)) LOCS = planar2lonlat(LOCS, p$aegis_proj4string_planar_km)
        LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
        st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
        LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )

        raster_template = raster( LOCS, res=min(p$gridparams$res), crs=st_crs( LOCS ) )
        for (vnm in variable_name) {
          LL = fasterize::fasterize( LU, raster_template, field=vnm )
          o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
          st_crs(o) = st_crs( LOCS )
          LOCS[,vnm] = st_drop_geometry(o)[ match(
            array_map( "xy->1", st_coordinates(LOCS), gridparams=p$gridparams ),
            array_map( "xy->1", st_coordinates(o), gridparams=p$gridparams )
          ), "layer" ]
        }

        return( st_drop_geometry(LOCS)[,variable_name] )
      }


      if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
        # areal unit (LU) to areal unit (LOCS)
        AU = areal_units( p=p )  #  poly associated with AU
        bm = match( AU$AUID, LU$space )
        
        # now rasterize and re-estimate
        raster_template = raster( LOCS, res=min(p$gridparams$res), crs=st_crs( LOCS ) )
        for (vnm in variable_name) {
          LL = fasterize::fasterize( LU[[vnm]][ bm, "mean"], raster_template, field=vnm )
          o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
          st_crs(o) = st_crs( LOCS )
          LOCS[, vnm] = sf:::aggregate.sf( o, LOCS, FUNC, na.rm=TRUE ) [["layer"]]
        }
        return( st_drop_geometry(LOCS)[,variable_name] )
      }

    }


    if (p$aegis_dimensionality =="space-year" ) {


      if ( lookup_from %in% c("core") & lookup_to == "points" )  {
        # matching to point (LU ) to point (LOCS)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
      
        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year( LOCS$timestamp ) 
#        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        LU_map = paste( 
          array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams ), 
          array_map( "ts->year_index", LU[,c("yr")], dims=c(p$ny), res=c( 1  ), origin=c( min(p$yrs) ) ), 
          sep="_" 
        )

        LOCS_map = paste(
          array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ), 
          array_map( "ts->year_index", LOCS[, c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )

        LOCS[[ variable_name ]] = LU[ match( LOCS_map, LU_map ), variable_name ]

        return( LOCS[[ variable_name ]] )
      }


      if ( lookup_from %in% c("core") & lookup_to == "areal_units" )  {
        # point (LU) -> areal unit (LOCS: AU/timestamp)

        if (!exists("AU")) AU = areal_units( p=p )
        if (!exists("AUID", AU)) AU$AUID = as.character(1:nrow(AU))
        if (!exists("AUID", LOCS) | !exists("timestamp", LOCS) ) stop("require AUID and timestamp in LOCS") 

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
 
        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(AU) )
        LU_map = paste( 
          st_points_in_polygons( pts=LU, polys=AU[, "AUID"], varname= "AUID" ), 
          array_map( "ts->year_index", st_drop_geometry( LU) [,c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )

        if (!exists("lon", LOCS)) LOCS = planar2lonlat(LOCS, proj.type=p$aegis_proj4string_planar_km)  
        
        LOCS = st_as_sf( LOCS, coords=c("lon","lat") )
        st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
        LOCS = sf::st_transform( LOCS, crs=st_crs(AU) )
        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->year_index", st_drop_geometry(LOCS)[ , c("yr") ], dims=c(p$ny), res=c( 1 ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )

        LOCS_regridded = tapply( st_drop_geometry(LU)[, variable_name], LU_map, FUN=FUNC, na.rm=TRUE )

        LOCS[[ variable_name ]] = LOCS_regridded[ match( LOCS_map, as.character( names( LOCS_regridded )) ) ]

        return( LOCS[[ variable_name ]] )
      }


      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {
        

        # matching to point (LU) to points (LOCS)
        if (!exists("plon", LU))  LU = lonlat2planar(LU, proj.type=p$aegis_proj4string_planar_km)
        LU_map = array_map( "xy->1", LU[, c("plon","plat")], gridparams=p$gridparams )
        
        if (!exists("plon", LOCS))  LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        LOCS_map = array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams )
        LOCS_index = match( LOCS_map, LU_map )

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        # TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
        TIMESTAMP_index = array_map( "ts->year_index", LOCS [, c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) )


        return( LOCS[ cbind( LOCS_index, TIMESTAMP_index ) ] )
      }


      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
        # points (LU) -> areal units (LOCS)
        if (!exists("lon", LU)) LU = planar2lonlat(LU, p$aegis_proj4string_planar_km)
        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(LOCS) )

        if (!exists("AUID", AU_target)) AU_target$AUID = as.character(1:nrow(AU_target))

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
        # TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
        TIMESTAMP_index = array_map( "ts->year_index", LOCS [, c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) )

    
    
        # now rasterize and re-estimate
        LOCS$AU_index = match( LOCS$AUID, LU$AUID  )    # assuming AUID's are consistent
        
        # AU_target .... must be sent ... <---------
        AU_target = sf::st_transform( AU_target, crs=st_crs(p$aegis_proj4string_planar_km) )


      #   LOCS_AUID = st_points_in_polygons( pts=st_as_sf( LOCS, coords=c("lon","lat"), crs=crs_lonlat ), polys = LU[, "AUID"], varname= "AUID" )
        
      #   LOCS_map =  paste( 
      #     as.character( LOCS_AUID ),  
      #     array_map( "ts->year_index", LOCS[ , c("yr", "dyear") ], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
      #     sep="_"
      #   )

        LU_map = paste( 
          st_points_in_polygons( pts=LU, polys=AU_target[, "AUID"], varname= "AUID" ),
          array_map( "ts->year_index", LU[,c("yr", "dyear")], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )
        

      #   for (vnm in variable_name) {
      #     LOCS_regridded = tapply( LU[, vnm], LU_uid, FUN=FUNC, na.rm=TRUE )
      #     LOCS[ , vnm ] = LOCS_regridded[ match( LOCS_map, as.character( names( LOCS_regridded )) ) ]
      #   }
      #   return( st_drop_geometry(LOCS)[,variable_name] )
        stop("incomplete")
      }



      if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
        # areal unit (LU) to points (LOCS)

    #    nw = length(LU$dyear)
        ny = length(LU$time)
        yr0 = min(as.numeric(LU$time))

        AU = sf::st_transform( LU$sppoly, crs=st_crs(p$aegis_proj4string_planar_km) )
        AU$au_index = 1:nrow(AU)
        AU = st_cast(AU, "POLYGON")
              
        LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
        st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
        LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" )   
        LOCS$AU_index = match( LOCS$AUID, LU$space  )    
        
        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        # TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
        TIMESTAMP_index = array_map( "ts->year_index", LOCS[, c("yr" )], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )
      
        LOCS[[variable_name]] = LU[["predictions"]][ cbind( LOCS$AU_index, TIMESTAMP_index, "mean" )]

        return( LOCS[[variable_name]] )  
      
      }


      if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
        # areal unit (LU) to areal units (AU/LOCS) 

        # from source data: LU = modelled predictions; AU are associated areal units linked by "AUID" 
    #    nw = length(LU$dyear)
        ny = length(LU$time)
        yr0 = min(as.numeric(LU$time))

        AU = sf::st_transform( LU$sppoly, crs=st_crs(p$aegis_proj4string_planar_km) )
        AU = st_cast(AU, "POLYGON")
        AU$au_uid = 1:nrow(AU)
        
        # au_uid is internal index of AU /LU
        AU_raster = fasterize::fasterize( AU, raster::raster( AU, res=min(p$gridparams$res)/2, crs=st_crs( AU ) ), field="au_uid" )  
        AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(AU_raster)), coords=c("x", "y") )
        st_crs(AU_pts) = st_crs( AU ) 

        pts_AU = match(AU_pts$layer, AU$au_uid[match(LU$space, AU$AUID)] ) ## (layer==au_uid) -- to -- LU

        # to target locations: AU_target 
        AU_target = sf::st_transform( AU_target, crs=st_crs(AU) )  # AU_target .... must be sent ... <---------
        AU_target = st_cast( AU_target, "POLYGON")

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        # LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
        
        # TIMESTAMP_index = array_map( "ts->2", LOCS [, c("yr", "dyear")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
        TIMESTAMP_index = array_map( "ts->year_index", LOCS [, c("yr" )], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )

        # id membership in AU_target
        pts_AUID = st_points_in_polygons( pts=AU_pts, polys=AU_target[,"AUID"], varname="AUID" ) 

        LOCS_regridded = apply( 
          LU$predictions[,,"mean"], 
          MARGIN=c(2), 
          FUN=function( LUV ) {
            tapply(X=LUV[ pts_AU ], INDEX=pts_AUID, FUN=FUNC, na.rm=TRUE) 
          }
        )
        space_index = match( LOCS$AUID, as.numeric(as.character(dimnames(LOCS_regridded )[[1]] ))  )   # AUID of AU_target (from pts_AUID)
        LOCS[[variable_name]] = LOCS_regridded[ cbind( space_index, TIMESTAMP_index ) ]

        return( LOCS[[variable_name]] )
    
      } 

    }
    
    
    ######################################################


    if (p$aegis_dimensionality =="space-year-season" ) {

      if ( lookup_from %in% c("core") & lookup_to == "points" )  {
        setDT(LOCS)
        if (!exists("plon", LOCS))  LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        
        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year( LOCS$timestamp ) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        LU_map = paste( 
          array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams ), 
          array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_" 
        )

        LOCS_map = paste(
          array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ), 
          array_map( "ts->1", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )

        LOCS[[ variable_name ]] = LU[ match( LOCS_map, LU_map ), variable_name ]

        return( LOCS[[ variable_name ]] )
      }


      if ( lookup_from %in% c("core") & lookup_to == "areal_units" )  {
 
        if (!exists("AU")) AU = areal_units( p=p )
        if (!exists("AUID", AU)) AU$AUID = as.character(1:nrow(AU))
        if (!exists("AUID", LOCS) | !exists("timestamp", LOCS) ) stop("require AUID and timestamp in LOCS") 

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr


        if (!exists("plon", LU))  LU = lonlat2planar(LU, proj.type=p$aegis_proj4string_planar_km)

        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(AU) )
        LU_map = paste( 
          st_points_in_polygons( pts=LU, polys=AU[, "AUID"], varname= "AUID" ), 
          array_map( "ts->1", st_drop_geometry( LU) [,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )
        
        LOCS = st_as_sf( LOCS, coords=c("lon","lat") )
        st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
        LOCS = sf::st_transform( LOCS, crs=st_crs(AU) )
        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->1", st_drop_geometry(LOCS)[ , c("yr", "dyear") ], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )

        LOCS_regridded = tapply( st_drop_geometry(LU)[, variable_name], LU_map, FUN=FUNC, na.rm=TRUE )

        LOCS[[variable_name ]] = LOCS_regridded[ match( LOCS_map, as.character( names( LOCS_regridded )) ) ]

        return( LOCS[[ variable_name ]] )
      }


      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "points" )  {

        # matching to point (LU) to points (LOCS)
        if (!exists("plon", LU))  LU = lonlat2planar(LU, proj.type=p$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS))  LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km

        LU_map = array_map( "xy->1", LU[, c("plon","plat")], gridparams=p$gridparams )
        
        LOCS_map = array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams )
        LOCS_index = match( LOCS_map, LU_map )

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        TIMESTAMP_index = array_map( "ts->2", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )

        return( LOCS[ cbind( LOCS_index, TIMESTAMP_index ) ] )
      }


      if ( lookup_from %in% c("stmv", "hybrid") & lookup_to == "areal_units" )  {
        # points (LU) -> areal units (LOCS)
        if (!exists("lon", LU)) LU = planar2lonlat(LU, p$aegis_proj4string_planar_km)
        
        LU = sf::st_as_sf( LU, coords=c("lon", "lat") )
        st_crs(LU) = st_crs( projection_proj4string("lonlat_wgs84") )
        LU = sf::st_transform( LU, crs=st_crs(LOCS) )

        if (!exists("AUID", AU_target)) AU_target$AUID = as.character(1:nrow(AU_target))

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$yr = lubridate::year(LOCS$timestamp) 
        LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        # now rasterize and re-estimate
        LOCS$AU_index = match( LOCS$AUID, LU$AUID  )    # assuming AUID's are consistent
        
        # AU_target .... must be sent ... <---------
        AU_target = sf::st_transform( AU_target, crs=st_crs(p$aegis_proj4string_planar_km) )

        LU_map = paste( 
          st_points_in_polygons( pts=LU, polys=AU_target[, "AUID"], varname= "AUID" ),
          array_map( "ts->1", LU[,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )

        stop("incomplete")
      }



      if ( lookup_from %in% c("carstm" ) & lookup_to == "points" )  {
        # areal unit (LU) to points (LOCS)
   
        nw = length(LU$season)
        ny = length(LU$time)
        yr0 = min(as.numeric(LU$time))

        AU = sf::st_transform( LU$sppoly, crs=st_crs(p$aegis_proj4string_planar_km) )
        AU$au_index = 1:nrow(AU)
        AU = st_cast(AU, "POLYGON")

        LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
        st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
        LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = AU[, "AUID"], varname= "AUID" )   
        LOCS$AU_index = match( LOCS$AUID, LU$space  )    
        
        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$time = lubridate::year(LOCS$timestamp) 
        LOCS$season = lubridate::decimal_date( LOCS$timestamp ) - LOCS$time
        TIMESTAMP_index = array_map( "ts->2", st_drop_geometry(LOCS) [, c("time", "season")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
      
        LOCS[[variable_name]] = LU[["predictions"]][ cbind( LOCS$AU_index, TIMESTAMP_index, "mean" )]

        return( LOCS[[variable_name]] )  
      
      }


      if ( lookup_from %in% c("carstm") & lookup_to == "areal_units" )  {
        # areal unit (LU) to areal units (AU/LOCS) 
        # from source data: LU = modelled predictions; AU are associated areal units linked by "AUID" 
     
        nw = length(LU$season)
        ny = length(LU$time)
        yr0 = min(as.numeric(LU$time))

        AU = sf::st_transform( LU$sppoly, crs=st_crs(p$aegis_proj4string_planar_km) )
        AU = st_cast(AU, "POLYGON")
        AU$au_uid = 1:nrow(AU)
        
        # au_uid is internal index of AU /LU
        AU_raster = fasterize::fasterize( AU, raster::raster( AU, res=min(p$gridparams$res)/2, crs=st_crs( AU ) ), field="au_uid" )  
        AU_ps = sf::st_as_sf( as.data.frame( raster::rasterToPoints(AU_raster)), coords=c("x", "y") )
        st_crs(AU_ps) = st_crs( AU ) 

        ps_AU = match(AU_ps$layer, AU$au_uid[match(LU$space, AU$AUID)] ) ## (layer==au_uid) -- to -- LU

        # to target locations: AU_target 
        AU_target = sf::st_transform( AU_target, crs=st_crs(AU) )  # AU_target .... must be sent ... <---------
        AU_target = st_cast( AU_target, "POLYGON")

        if (! "POSIXct" %in% class(LOCS$timestamp)  ) LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        LOCS$time = lubridate::year(LOCS$timestamp) 
        LOCS$season = lubridate::decimal_date( LOCS$timestamp ) - LOCS$time
        TIMESTAMP_index = array_map( "ts->2", LOCS [, c("time", "season")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
        
        # id membership in AU_target
        ps_AUID = st_points_in_polygons( pts=AU_ps, polys=AU_target[,"AUID"], varname="AUID" ) 

        time_index = array_map( "ts->2", LOCS[ , c("time", "season") ], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )

        LOCS_regridded = apply( 
          LU$predictions[,,,"mean"], 
          MARGIN=c(2,3), 
          FUN=function( LUV ) {
            tapply(X=LUV[ ps_AU ], INDEX=ps_AUID, FUN=FUNC, na.rm=TRUE) 
          }
        )
        space_index = match( LOCS$AUID, as.numeric(as.character(dimnames(LOCS_regridded )[[1]] ))  )   # AUID of AU_target (from ps_AUID)

        LOCS[[variable_name]] = LOCS_regridded[ cbind( space_index, time_index ) ]

        return( LOCS[[variable_name]] )
    
      } 

   } # end space-year-season

  } # end for data_class
  
}
