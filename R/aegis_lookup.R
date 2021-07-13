
aegis_lookup = function( 
  data_class="bathymetry", 
  variable_name=NULL,
  variabletomodel=NULL,  # required only for projects with multiple variables to model such as speciescomposition 
  LUT = NULL,    # look up table from which to obtain results
  LOCS = NULL,   # look up locations for which results are desired
  LUT_AU=NULL,   # areal units associated with LUT
  LOCS_AU=NULL, 
  project_class="core",   # "project_class" to look up from
  DS="aggregated_data", # "DS=.. "
  output_format="points", 
  vnm = "predictions",  # used for carstm lookups
  statvars = c("mean"),
  tz="America/Halifax", 
  year.assessment=NULL , 
  FUNC=mean, 
  returntype = "vector",
  raster_resolution=1, ...
) {
 

# join = function(x, y) st_is_within_distance(x, y, dist = raster_resolution )
# join = function(x, y) st_is_within_distance(x, y, dist = raster_resolution )
# join = st_intersects


 if (0) {
   # testing usign snow data
    if (0) {
      # generics
      data_class="bathymetry" 
      variable_name=list( "predictions", c("random", "space", "combined") )
      variabletomodel="pca1"  # required only for projects with multiple variables to model such as speciescomposition 
      LUT = NULL    # look up table from which to obtain results
      LOCS = M[, c("lon", "lat", "timestamp")]   # look up locations for which results are desired
      LUT_AU=NULL   # areal units associated with LUT
      LOCS_AU=sppoly 
      project_class="core"   # "project_class" to look up from
      DS="aggregated_data" # "DS=.. "
      output_format="points" 
      vnm = "predictions"  # used for carstm lookups
      statvars = c("mean")
      tz="America/Halifax" 
      year.assessment= lubridate::year(lubridate::now())   
      FUNC=mean 
      join = st_intersects 
      raster_resolution=1
  }

    year.assessment = 2020

    p = bio.snowcrab::snowcrab_parameters(
      project_class="carstm",
      assessment.years=2000:year.assessment,
      areal_units_type="tesselation",
      carstm_model_label = "tesselation",   # default is the name of areal_units_type
      selection = list(type = "number")
    )

    M = snowcrab.db( p=p, DS="biological_data" )  
   
    M = M[ which( M$yr %in% 2011:2015 ) ,]  # reduce data size
    dim(M)

    sppoly = areal_units( p=p )  # poly to estimate a surface 

    loadfunctions("aegis")
    debug(aegis_lookup)

    
    # test raw data lookup
    # spatial
    o1 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "z.mean", "z.sd", "z.n"),
      returntype="sf" 
    ) 

    o1 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "z.mean" ),
      returntype="vector"
    ) 

    o2 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "z", "dZ", "ddZ") 
    ) 
 
    # spatial averaging by areal unit
    # this one is slow .. could be sped up if used 
    o3 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="stmv", output_format="areal_units" , DS="complete", variable_name=c( "z", "dZ", "ddZ") 
    ) 

    # spatial averaging by areal unit
    o4 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="core", output_format="areal_units" , DS="complete", variable_name=c( "z.mean", "z.sd", "z.n") 
    ) 

    # lookup from areal unit predictions
    o5 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res)
    ) 

    # slow 
    o6 = aegis_lookup(  data_class="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res) /2
    ) 

    # au to au match (LOCS=NULL)
    o7 = aegis_lookup(  data_class="bathymetry",  LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res) /2
    ) 

    # consistency checks
    plot(o1$z ~o2$z)
    plot(o1$z ~o3$z)
    plot(o1$z ~o4$z)
    plot(o1$z ~o5$predictions_mean) 
    plot(o1$z ~o6$predictions_mean) 


    # space-time
    o1 = aegis_lookup(  data_class="speciescomposition", LOCS=M[, c("lon", "lat", "timestamp")], variabletomodel="pca1",  
      project_class="core", output_format="points" , DS="speciescomposition", variable_name=c( "pca1" ) 
    ) 

    o2 = aegis_lookup(  data_class="speciescomposition", LOCS=M[, c("lon", "lat", "timestamp")], variabletomodel="pca1",  
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "pca1") 
    ) 

    o3 = aegis_lookup(  data_class="speciescomposition", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly, variabletomodel="pca1",
      project_class="core", output_format="areal_units" ,  variable_name=list( "pca1",  "pca2", "ca1", "ca2" )
    ) 
 
    o4 = aegis_lookup(  data_class="speciescomposition", LOCS=M[, c("lon", "lat", "timestamp")], variabletomodel="pca1",
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd")
    ) 
  
    o5 = aegis_lookup(  data_class="speciescomposition", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly, variabletomodel="pca1", 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res)/2
    ) 

    mm = expand.grid(AUID=sppoly$AUID, timestamp=lubridate::date_decimal( p$yrs, tz="America/Halifax" ))

    o6 = aegis_lookup(  data_class="speciescomposition", LOCS=mm, LOCS_AU=sppoly, variabletomodel="pca1", 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res)/2
    ) 


    # consistency checks
    plot(o1$pca1.mean ~o3$pca1.mean)
    plot(o1$pca1.mean ~o4$predictions_mean) 
    plot(o1$pca1.mean ~o5$predictions_mean) 


    # space-time-season
    o1 = aegis_lookup(  data_class="temperature", LOCS=M[, c("lon", "lat", "timestamp")],   
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "t.mean", "t.sd", "t.n")  
    ) 

    o2 = aegis_lookup(  data_class="temperature", LOCS=M[, c("lon", "lat", "timestamp")],    
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "t") 
    ) 

    o3 = aegis_lookup(  data_class="temperature", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly,  
      project_class="core", output_format="areal_units" ,  variable_name=list( "t.mean",  "t.sd", "t.n"  )
    ) 
 
    o4 = aegis_lookup(  data_class="temperature", LOCS=M[, c("lon", "lat", "timestamp")], 
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd")
    ) 
  
    o5 = aegis_lookup(  data_class="temperature", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly, 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res)/2
    ) 

    mm = expand.grid(AUID=sppoly$AUID, timestamp=lubridate::date_decimal( p$yrs, tz="America/Halifax" ))
    o6 = aegis_lookup(  data_class="temperature", LOCS=mm, LOCS_AU=sppoly, 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), raster_resolution=min(p$gridparams$res)/2
    ) 

    # consistency checks
    plot(o1$t.mean ~o3$t.mean)
    plot(o1$t.mean ~o4$predictions_mean) 
    plot(o1$t.mean ~o5$predictions_mean) 



    if (0) {
      p = bathymetry_parameters(  project_class="stmv"  )
      LUT = bathymetry_db ( p=p, DS="complete" )   
    }
 
  }
 
    if (!is.null(LOCS)) setDT(LOCS)
 
    require(data.table)  # enforce
    require(raster) # TODO use sf /stars /fasterize instead ..

    crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))
  
    # determine LUT (lookup table)
    LUT = NULL

    if ( "bathymetry" %in% data_class ) {

      p = bathymetry_parameters(  project_class=project_class  )
      if (is.null(LUT)) {
        if ( project_class %in% c("core" ) )  LUT = bathymetry_db ( p=p, DS=DS )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
        if ( project_class %in% c("stmv", "hybrid") ) LUT = bathymetry_db ( p=p, DS="complete" )   
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=p, DS="carstm_modelled_summary" ) 
      }
    }

    if ( "substrate" %in% data_class ) {
      p = substrate_parameters(  project_class=project_class  )
      if (is.null(LUT)) {
        if ( project_class %in% c("core" ) )  LUT = substrate_db ( p=p, DS=DS )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
        if ( project_class %in% c("stmv", "hybrid") ) {
          LUT = substrate_db ( p=p, DS="complete" )   
          pB = bathymetry_parameters( spatial_domain=p$spatial_domain, project_class=project_class  )
          BA = bathymetry_db ( p=pB, DS="baseline", varnames=c("lon", "lat")  )
          LUT = cbind(LUT, BA )
        }
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=p, DS="carstm_modelled_summary" ) 
      }
    }

    if ( "temperature" %in% data_class ) {
      if (is.null(year.assessment)) year.assessment = max( lubridate::year(lubridate::date_decimal( LOCS$timestamp, tz=tz )) )
      p = temperature_parameters(  project_class=project_class, year.assessment=year.assessment )
      if (is.null(LUT)) {
        if ( project_class %in% c("core" ) )  LUT = temperature_db ( p=p, DS=DS )  # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
        if ( project_class %in% c("stmv", "hybrid") )  LUT = temperature_db ( p=p, DS="complete" ) 
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=p, DS="carstm_modelled_summary" ) 
      }
    }


    if ( data_class %in% c("speciescomposition", "speciescomposition_pca1", "speciescomposition_pca2", "speciescomposition_ca1", "speciescomposition_ca2")  ){
      if (is.null(year.assessment)) year.assessment = max( lubridate::year(lubridate::date_decimal( LOCS$timestamp, tz=tz )) )
      if (is.null(variabletomodel)) {
        if (data_class == "speciescomposition_pca1") variabletomodel = "pca1" 
        if (data_class == "speciescomposition_pac2") variabletomodel = "pca2" 
        if (data_class == "speciescomposition_ca1") variabletomodel = "ca1" 
        if (data_class == "speciescomposition_ca2") variabletomodel = "ca2" 
      }
      p = speciescomposition_parameters(  project_class=project_class, variabletomodel=variabletomodel, year.assessment=year.assessment  )
      if (is.null(LUT)) {
        if ( project_class %in% c("core" ) ) LUT = speciescomposition_db ( p=p, DS=DS )   # "aggregated_data", "bottom.all" , "spatial.annual.seasonal", "complete"
        if ( project_class %in% c( "stmv", "hybrid") )  LUT = aegis_db( p=p, DS="complete" )   
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=p, DS="carstm_modelled_summary" ) 
      }
    }

    if (is.null(LUT)) stop( "lookup data not found nor given")

    if (is.null(variable_name)) variable_name = setdiff( names(LUT), c("plon", "plat", "lon", "lat", "timestamp", "year", "dyear", "yr" ))
 
    # ------------------

    if (p$aegis_dimensionality =="space" ) {

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {

        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=p$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km

        gridparams = p$gridparams 
        gridparams$res = c(raster_resolution, raster_resolution)
 
        ii = match(
              array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=gridparams ),
              array_map( "xy->1", LUT[,  c("plon","plat")], gridparams=gridparams )
        )
        for (vnm in variable_name) {
          if ( vnm %in% names(LUT ))  LOCS[[vnm]] = LUT[ ii, vnm ]
        } 

      }

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(p$aegis_proj4string_planar_km) 
        } 

        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT) )

        LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
 
        variable_name = intersect( names(LUT), variable_name )
        LUT = LUT[, variable_name]

        # for (vnm in variable_name) {
        #   LOCS[[vnm]] =  sf:::aggregate.sf( LUT[, vnm], LOCS, FUNC, na.rm=TRUE ) [[vnm]]
        # }
        LU_map =  st_points_in_polygons( pts=LUT, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
        LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {  
            LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LOCS[[ vnm ]] = LUT_regridded[ match( LOCS$AUID, as.character( names( LUT_regridded )) ) ]
          }   
        }

      }

 
      if ( project_class %in% c("carstm" ) & output_format == "points" )  {

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )
        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  

        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(p$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   

        raster_template = raster::raster( LUT_AU, res=raster_resolution, crs=st_crs( p$aegis_proj4string_planar_km ) )
        gridparams = p$gridparams 
        gridparams$res = c(raster_resolution, raster_resolution)
        # prep-pass with a au_index variable to get index
        LUT_AU$au_index = 1:nrow(LUT_AU)
        LL = fasterize::fasterize( LUT_AU, raster_template, field="au_index" )
        o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
        st_crs(o) = st_crs( LUT_AU )
        ii = match(
          array_map( "xy->1", st_coordinates(LOCS), gridparams=gridparams ),
          array_map( "xy->1", st_coordinates(o), gridparams=gridparams )
        )
        LUT$au_index = NULL
        LL = o = NULL 
        gc()

        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              LUT_AU[[vnm]] = carstm_results_unpack( LUT, vnh ) [ bm, stat_var ]
              LL = fasterize::fasterize( LUT_AU, raster_template, field=vnm )
              o = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LL)), coords=c("x", "y") )
              st_crs(o) = st_crs( LUT_AU )
              LOCS[[vnm]] = o[["layer"]][ ii ]
            }
          }
        } 

      }



      if ( project_class %in% c("carstm") & output_format == "areal_units" )  {
       
        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )

        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster::raster( LUT_AU, res=raster_resolution, crs=st_crs( LUT_AU ) ), field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (is.null(LOCS)) {
          # matching directly to LOCS_AU
          LOCS = LOCS_AU
          if (!exists("AUID", LOCS) ) stop( message("AUID must be found in LOCS or LOCS_AU"))

        } else {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
            LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
          }

        }
        
        # covert LUT_AU to LOCS_AU
        # id membership of LOCS in LOCS_AU

        # id membership of LUT raster in LOCS_AU
        LUT_AU_pts_LOCS_AU_AUID = st_points_in_polygons( pts=LUT_AU_pts, polys=LOCS_AU[,"AUID"], varname="AUID" ) 
  
        pts_AU = match(LUT_AU_pts$layer, LUT_AU$lut_uid[match(LUT$space, LUT_AU$AUID)] ) ## (layer==lut_uid) -- to -- LUT

        aggFUN = function( LUV ) tapply(X=LUV, INDEX=LUT_AU_pts_LOCS_AU_AUID, FUN=FUNC, na.rm=TRUE) 

        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              o = carstm_results_unpack( LUT, vnh ) 
              kk = length(dim(o))
              if (kk == 2) {
                ov = o[ pts_AU,  which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = aggFUN( ov)
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[ space_index  ]
              }
            }
          } 
        }
      }

    }
 

    ######################################################
 
    if (p$aegis_dimensionality =="space-year" ) {


      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {

        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=p$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
      
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year( LOCS$timestamp ) 
 
        ii = match( 
          paste(
            array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ), 
            array_map( "ts->year_index", LOCS[, c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
            sep="_"
          ), 
          paste( 
            array_map( "xy->1", LUT [,c("plon","plat")],  gridparams=p$gridparams ), 
            array_map( "ts->year_index", LUT[,c("yr")],    dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
            sep="_" 
          ) 
        )
        
        for (vnm in variable_name) {
          if ( vnm %in% names(LUT ))  LOCS[[vnm]] = LUT[ ii, vnm ]
        }

      }


      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(p$aegis_proj4string_planar_km) 
        } 

        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if ( ! "sf" %in% class(LOCS) ) {
          LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
        }

        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT) )

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 

        LU_map = paste( 
          st_points_in_polygons( pts=LUT, polys=LOCS_AU[, "AUID"], varname= "AUID" ), 
          array_map( "ts->year_index", st_drop_geometry( LUT) [,c("yr" )], dims=c(p$ny ), res=c( 1  ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )

        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = LOCS_AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->year_index", st_drop_geometry(LOCS)[ , c("yr") ], dims=c(p$ny), res=c( 1 ), origin=c( min(p$yrs) ) ), 
          sep="_"
        )

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT ))  {
            LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LOCS[[ vnm ]] = LUT_regridded[ match( LOCS_map, as.character( names( LUT_regridded )) ) ]
          }
        }

      }

 


      if ( project_class %in% c("carstm" ) & output_format == "points" )  {
        
        # discretize LUT_AU to LOCS_AU and then re-estimate means by LOCS_AU$AUID then tag each estimate to AUID of LOCS

        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )
        LUT_AU$au_index = 1:nrow(LUT_AU)
        if (!exists("AUID", LUT_AU)) {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LUT_AU$AUID = as.character(LUT_AU$au_index)
        }
        bm = match( LUT_AU$AUID, LUT$space )  
              
        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(p$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   
       
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
      
      
        ii = cbind( 
          match( LOCS$AUID, LUT$space[bm] )
        )

        jj = cbind( 
          match( LOCS$AUID, LUT$space[bm] ), 
          array_map( "ts->year_index", LOCS[["yr"]], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) ) 
        )

        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              o = carstm_results_unpack( LUT, vnh ) 
              kk = length(dim(o))
              if (kk == 2) LOCS[[vnm]] = o[ cbind(ii, which(dimnames(o)$stat == stat_var) )  ] 
              if (kk == 3) LOCS[[vnm]] = o[ cbind(jj, which(dimnames(o)$stat == stat_var) )  ] 
            }
          } 
        }

      }


      if ( project_class %in% c("carstm") & output_format == "areal_units" )  {

        # discretize LUT_AU to LOCS_AU and then re-estimate means by LOCS_AU$AUID  then tag each estimate to AUID of LOCS

        # from source data: LUT = modelled predictions; LUT_AU are associated areal units linked by "AUID" 
        #    nw = length(LUT$dyear)

        # format coordinate systems   
        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )
        
        # dims
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster::raster( LUT_AU, res=raster_resolution, crs=st_crs( LUT_AU ) ), field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (is.null(LOCS)) {
          # matching directly to LOCS_AU
          LOCS = LOCS_AU
          if (!exists("AUID", LOCS) ) stop( message("AUID must be found in LOCS or LOCS_AU"))

        } else {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
            LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
          }
        }

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        
        TIMESTAMP_index1 = array_map( "ts->year_index", LOCS[["yr"]], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )

        # id membership of LUT raster in LOCS_AU
        LUT_AU_pts_LOCS_AU_AUID = st_points_in_polygons( pts=LUT_AU_pts, polys=LOCS_AU[,"AUID"], varname="AUID" ) 
  
        pts_AU = match(LUT_AU_pts$layer, LUT_AU$lut_uid[match(LUT$space, LUT_AU$AUID)] ) ## (layer==lut_uid) -- to -- LUT
        aggFUN = function( LUV ) {
                  tapply(X=LUV, INDEX=LUT_AU_pts_LOCS_AU_AUID, FUN=FUNC, na.rm=TRUE) 
        }
 
        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              o = carstm_results_unpack( LUT, vnh ) 
              kk = length(dim(o))
              if (kk == 2) {
                ov = o[ pts_AU,  which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = aggFUN( ov)
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[  space_index  ]
              }
              if (kk == 3) {
                ov = o[ pts_AU,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index1 ) ]
              }
            }
          } 
        }
      }

    }  # end dimension
    
    ######################################################


    if (p$aegis_dimensionality =="space-year-season" ) {

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {

        if (!exists("plon", LOCS))  LOCS = lonlat2planar(LOCS, proj.type=p$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=p$aegis_proj4string_planar_km)
        
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year( LOCS$timestamp ) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr

        if (0) {
          # testing: 
          LU_map = array_map( "xy->1", LUT[, c("plon","plat")], gridparams=p$gridparams )
          LOCS_map = array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams )
          LOCS_index = match( LOCS_map, LU_map )
          TIMESTAMP_index2 = array_map( "ts->2", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) )
          LOCS[[vnm]] = LUT[ cbind( LOCS_index, TIMESTAMP_index2 ), vnm ]
        }

        ii = match( 
          paste(
            array_map( "xy->1", LOCS[, c("plon","plat")], gridparams=p$gridparams ), 
            array_map( "ts->1", LOCS[, c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
            sep="_"
          ), 
          paste( 
            array_map( "xy->1", LUT[,c("plon","plat")], gridparams=p$gridparams ), 
            array_map( "ts->1", LUT[,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
            sep="_" 
          ) 
        )

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT ))  LOCS[[ vnm ]] = LUT[ ii, vnm ]
        }

      }

 
      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {
 

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(p$aegis_proj4string_planar_km) 
        } 

        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if ( ! "sf" %in% class(LOCS) ) {
          LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
        }

        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT) )

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) )LOCS$yr = lubridate::year(LOCS$timestamp) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
 

        LU_map = paste( 
          st_points_in_polygons( pts=LUT, polys=LOCS_AU[, "AUID"], varname= "AUID" ), 
          array_map( "ts->1", st_drop_geometry( LUT) [,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )
 
        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = LOCS_AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->1", st_drop_geometry(LOCS)[ , c("yr", "dyear") ], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
          sep="_"
        )

        if (0) {
            LOCS$AU_index = match( LOCS$AUID, LUT$AUID  )    # assuming AUID's are consistent
            
            # LOCS_AU .... must be sent ... <---------
            LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(p$aegis_proj4string_planar_km) )

            LU_map = paste( 
              st_points_in_polygons( pts=LUT, polys=LOCS_AU[, "AUID"], varname= "AUID" ),
              array_map( "ts->1", LUT[,c("yr", "dyear")], dims=c(p$ny, p$nw), res=c( 1, 1/p$nw ), origin=c( min(p$yrs), 0) ), 
              sep="_"
            )
        }

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {
            LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LOCS[[vnm ]] = LUT_regridded[ match( LOCS_map, as.character( names( LUT_regridded )) ) ]
          }
        }

      }



      if ( project_class %in% c("carstm" ) & output_format == "points" )  {
        # areal unit (LUT) to points (LOCS)
   
        nw = length(LUT$season)
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )
        LUT_AU$au_index = 1:nrow(LUT_AU)
        if (!exists("AUID", LUT_AU)) {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LUT_AU$AUID = as.character(LUT_AU$au_index)
        }
        bm = match( LUT_AU$AUID, LUT$space )  

        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(p$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
      
     
        ii = cbind( 
          match( LOCS$AUID, LUT$space[bm] )
        )

        jj = cbind( 
          match( LOCS$AUID, LUT$space[bm] ), 
          array_map( "ts->year_index", LOCS[["yr"]], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) ) 
        )

  
        LOCS_DF = LOCS
        if (inherits(LOCS_DF, "sf")) LOCS_DF = st_drop_geometry(LOCS_DF)

        ll = cbind( 
          match( LOCS$AUID, LUT$space[bm] ), 
          array_map( "ts->2", LOCS_DF[, c("yr", "dyear")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) ) 
        )
        LOCS_DF = NULL

        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              o = carstm_results_unpack( LUT, vnh ) 
              kk = length(dim(o))
              if (kk == 2) LOCS[[vnm]] = o[ cbind(ii, which(dimnames(o)$stat == stat_var) )  ] 
              if (kk == 3) LOCS[[vnm]] = o[ cbind(jj, which(dimnames(o)$stat == stat_var) )  ] 
              if (kk == 4) LOCS[[vnm]] = o[ cbind(ll, which(dimnames(o)$stat == stat_var) )  ] 
            }
          } 
        }
      
      }


      if ( project_class %in% c("carstm") & output_format == "areal_units" )  {
        # areal unit (LUT) to areal units (LUT_AU/LOCS) 
        # from source data: LUT = modelled predictions; LUT_AU are associated areal units linked by "AUID" 
     
        # format coordinate systems   
        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(p$aegis_proj4string_planar_km) )

        nw = length(LUT$season)
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster::raster( LUT_AU, res=raster_resolution, crs=st_crs( LUT_AU ) ), field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (is.null(LOCS)) {
          # matching directly to LOCS_AU
          LOCS = LOCS_AU
          if (!exists("AUID", LOCS) ) stop( message("AUID must be found in LOCS or LOCS_AU"))

        } else {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(p$aegis_proj4string_planar_km) )
            LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
          }

        }

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
    
        LOCS_DF = LOCS
        if (inherits(LOCS_DF, "sf")) LOCS_DF = st_drop_geometry(LOCS_DF)

        TIMESTAMP_index1 = array_map( "ts->year_index", LOCS_DF[, "yr"], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )
        TIMESTAMP_index2 = array_map( "ts->2", LOCS_DF[, c("yr", "dyear")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
        
        LOCS_DF = NULL

        # id membership of LUT raster in LOCS_AU
        LUT_AU_pts_LOCS_AU_AUID = st_points_in_polygons( pts=LUT_AU_pts, polys=LOCS_AU[,"AUID"], varname="AUID" ) 

        pts_AU = match(LUT_AU_pts$layer, LUT_AU$lut_uid[match(LUT$space, LUT_AU$AUID)] ) ## (layer==lut_uid) -- to -- LUT
        aggFUN = function( LUV ) {
                  tapply(X=LUV, INDEX=LUT_AU_pts_LOCS_AU_AUID, FUN=FUNC, na.rm=TRUE) 
        }
 
 
        for (g in 1:length(statvars)) {
          stat_var = statvars[g]
          for (h in 1:length(variable_name) ) {
            vnh = variable_name[[h]] 
            if (exists(vnh[[1]], LUT)) {
              vnm = paste( paste0(vnh,  collapse="_"),  stat_var, sep="_" )
              o = carstm_results_unpack( LUT, vnh ) 
              kk = length(dim(o))
              if (kk == 2) {
                ov = o[ pts_AU,  which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = aggFUN( ov)
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[  space_index  ]
              }
              if (kk == 3) {
                ov = o[ pts_AU,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index1 ) ]
              }
              if (kk == 4) {
                ov = o[ pts_AU,,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2,3), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                LOCS[[vnm]] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index2 ) ]
              }
            }
          } 
        }
    
      }
    } # end space-year-season


    if (returntype =="sf" ) {
      if (! inherits(LOCS, "sf"))   LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
    }
    if (returntype =="data.frame" ) {
      if  (! inherits(LOCS, "data.frame"))  LOCS = as.data.frame(LOCS)
    }

    if (returntype =="data.table" ) {
      if (! inherits(LOCS, "data.table"))   setDT(LOCS)
    }

    if (returntype =="vector" ) {
      if (length(variable_name) == 1) {
        if (exists( variable_name, LOCS)) LOCS = LOCS[[variable_name]] 
      } 
      message("variable_name is not a single variable, all being returned")
    }

    return(LOCS)

}
