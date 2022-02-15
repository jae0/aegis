
# TODO:: tapply can be converted into data.table computations for speed
## WARNING:: if you polygons are misspecified, they can result in some au's being dropped .. check your polygons

aegis_lookup = function( 
  parameters = NULL, 
  variable_name=NULL,
  LUT = NULL,    # look up table from which to obtain results
  LOCS = NULL,   # look up locations for which results are desired
  LUT_AU=NULL,   # areal units associated with LUT
  LOCS_AU=NULL, 
  project_class="core",   # "project_class" to look up from
  DS="aggregated_data", # "DS=.. "
  output_format="points", 
  statvars = c("mean"),
  tz="America/Halifax", 
  FUNC=mean, 
  returntype = "vector",
  space_resolution=NULL,   # spatial planar discretization
  time_resolution=NULL,  # year, seasonal discretization
  year.assessment=NULL, ...
) {


 if (0) {
   # testing usign snow data
    if (0) {
      # generics
      parameters="bathymetry" 
      variable_name=list( "predictions", c("random", "space", "combined") )
      LUT = NULL    # look up table from which to obtain results
      LOCS = M[, c("lon", "lat", "timestamp")]   # look up locations for which results are desired
      LUT_AU=NULL   # areal units associated with LUT
      LOCS_AU=sppoly 
      project_class="core"   # "project_class" to look up from
      DS="aggregated_data" # "DS=.. "
      output_format="points" 
      statvars = c("mean")
      tz="America/Halifax" 
      FUNC=mean 
      space_resolution=1
    }


    p = bio.snowcrab::snowcrab_parameters(
      project_class="carstm",
      yrs=1999:2021,
      areal_units_type="tesselation",
      carstm_model_label = "tesselation",   # default is the name of areal_units_type
      selection = list(type = "number")
    )

    M = snowcrab.db( p=p, DS="biological_data" )  
   
    M = M[ which( M$yr %in% 2011:2015 ) ,]  # reduce data size
    dim(M)

    sppoly = areal_units( p=p )  # poly to estimate a surface 

    # loadfunctions("aegis")
    # debug(aegis_lookup)
    
    # test raw data lookup
    # spatial
    o0 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "z.mean", "z.sd", "z.n"),
      returntype="sf" 
    ) 

    o1 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "z.mean" ),
      returntype="vector"
    ) 

    o2 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "z", "dZ", "ddZ") 
    ) 
 
    # spatial averaging by areal unit
    # this one is slow .. could be sped up if used 
    o3 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="stmv", output_format="areal_units" , DS="complete", variable_name=c( "z", "dZ", "ddZ") 
    ) 

    # spatial averaging by areal unit
    o4 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="core", output_format="areal_units" , DS="aggregated_data", variable_name=c( "z.mean", "z.sd", "z.n") 
    ) 

    # lookup from areal unit predictions
    o5 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres)
    ) 

    # slow 
    o6 = aegis_lookup(  parameters="bathymetry", LOCS=M[, c("lon", "lat")],  LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres) /2
    ) 

    # au to au match (LOCS=NULL)
    o7 = aegis_lookup(  parameters="bathymetry", LOCS=list(AUID=sppoly$AUID), LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres) /2
    ) 

    # au to au match (LOCS=NULL)
    o7 = aegis_lookup(  parameters="bathymetry", LOCS=sppoly[,"AUID"], LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres) /2
    ) 

    # au to au match (LOCS=NULL)
    o7 = aegis_lookup(  parameters="bathymetry", LOCS=sppoly$AUID, LOCS_AU=sppoly,
      project_class="carstm", output_format="areal_units", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres) /2
    ) 

    # consistency checks
    plot(o1 ~o2$z)
    plot(o1 ~o3$z)
    plot(o1 ~o4$z)
    plot(o1 ~o5$predictions_mean) 
    plot(o1 ~o6$predictions_mean) 


    # space-time
    o1 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=M[, c("lon", "lat", "timestamp")],   
      project_class="core", output_format="points" , DS="speciescomposition", variable_name=c( "pca1"), space_resolution=2  
    ) 

    o2 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=M[, c("lon", "lat", "timestamp")],   
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "pca1") 
    ) 

    o3 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly, 
      project_class="core", output_format="areal_units" ,  variable_name=list( "pca1",  "pca2", "ca1", "ca2" )
    ) 
 
    o4 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=M[, c("lon", "lat", "timestamp")], 
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd")
    ) 
  
    o5 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly,  
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres)/2
    ) 

    mm = expand.grid(AUID=sppoly$AUID, timestamp=lubridate::date_decimal( p$yrs, tz="America/Halifax" ))
    mm = expand.grid(AUID=sppoly$AUID, timestamp= p$yrs )

    o6 = aegis_lookup(  parameters="speciescomposition_pca1", LOCS=mm, LOCS_AU=sppoly,  
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres)/2
    ) 


    # consistency checks
    plot(o1$pca1.mean ~o3$pca1.mean)
    plot(o1$pca1.mean ~o4$predictions_mean) 
    plot(o1$pca1.mean ~o5$predictions_mean) 


    # space-time-season
    o1 = aegis_lookup(  parameters="temperature", LOCS=M[, c("lon", "lat", "timestamp")],   
      project_class="core", output_format="points" , DS="aggregated_data", variable_name=c( "t.mean", "t.sd", "t.n"), space_resolution=2  
    ) 

    o2 = aegis_lookup(  parameters="temperature", LOCS=M[, c("lon", "lat", "timestamp")],    
      project_class="stmv", output_format="points" , DS="complete", variable_name=c( "t") 
    ) 

    o3 = aegis_lookup(  parameters="temperature", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly,  
      project_class="core", output_format="areal_units" ,  variable_name=list( "t.mean",  "t.sd", "t.n"  )
    ) 
 
    o4 = aegis_lookup(  parameters="temperature", LOCS=M[, c("lon", "lat", "timestamp")], 
      project_class="carstm", output_format="points", variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd")
    ) 
  
    o5 = aegis_lookup(  parameters="temperature", LOCS=M[, c("lon", "lat", "timestamp")], LOCS_AU=sppoly, 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres)/2
    ) 


   # mm = expand.grid(AUID=sppoly$AUID, timestamp=lubridate::date_decimal( p$yrs, tz="America/Halifax" ))
    mm = expand.grid(AUID=sppoly$AUID, timestamp= p$yrs )

    o6 = aegis_lookup(  parameters="temperature", LOCS=mm, LOCS_AU=sppoly, 
      project_class="carstm", output_format="areal_units" , variable_name=list( "predictions", c("random", "space", "combined") ), statvars=c("mean", "sd"), space_resolution=min(p$pres)/2
    ) 

    # consistency checks
    plot(o1$t.mean ~o3$t.mean)
    plot(o1$t.mean ~o4$predictions_mean) 
    plot(o1$t.mean ~o5$predictions_mean) 



    if (0) {
      p = bathymetry_parameters(  project_class="stmv"  )
      LUT = bathymetry_db ( p=p, DS="complete" )   
    }
 
  }  ## END consistency checks

  # -----------

    if ( is.null(parameters) ) stop( "parameters is required")

    pL = NULL
    if ( class(parameters) == "character" ) {
      # then use generic defaults
      aegis_project = parameters
    } else {
      aegis_project = names(parameters)  
      if (length(aegis_project) > 1) {
        warning( "more than one parameter list sent, taking the first only")
        aegis_project = aegis_project[1]
      }
      pL = parameters[[aegis_project]]
    }

    if (is.vector(LOCS)) LOCS = list(AUID=LOCS)
    if (!is.null(LOCS)) setDT(LOCS)
 

    crs_lonlat =  st_crs(projection_proj4string("lonlat_wgs84"))
  

    # determine LUT (lookup table)
    if (is.null(LUT)) {

      if ( "bathymetry" %in% aegis_project ) {
        if ( is.null(pL) )  pL = bathymetry_parameters(  project_class=project_class  )
        if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
        if ( project_class %in% c("core" ) ) LUT = bathymetry_db ( p=pL, DS=DS ) 
        if ( project_class %in% c("stmv", "hybrid") ) LUT = bathymetry_db ( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 

          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            setDT(LUT)
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat) ]
            }
            setDF(LUT)
          }

      }

      if ( "substrate" %in% aegis_project ) {
        if ( is.null(pL) )  pL = substrate_parameters(  project_class=project_class  )
        if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
        if ( project_class %in% c("core" ) ) LUT = substrate_db ( p=pL, DS=DS ) 
        if ( project_class %in% c("stmv", "hybrid") ) {
          LUT = substrate_db ( p=pL, DS="complete" )   
          pB = bathymetry_parameters( spatial_domain=pL$spatial_domain, project_class=project_class  )
          BA = bathymetry_db ( p=pB, DS="baseline", varnames=c("lon", "lat")  )
          LUT = cbind( LUT, BA )
        }
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
                
          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            setDT(LUT)
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat) ]
            }
            setDF(LUT)
          }

      }

      if ( "temperature" %in% aegis_project ) {

        if (is.null(pL) )  pL = temperature_parameters(  project_class=project_class, year.assessment=year.assessment )
        if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
        if (is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
        if ( project_class %in% c("core" ))  LUT = temperature_db ( p=pL, DS=DS )  # "aggregated_data", "bottom.all"
        if ( project_class %in% c("stmv", "hybrid") )  LUT = temperature_db ( p=pL, DS="complete" ) 
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 
        
          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
          }
    
      }


      if ( grepl("speciescomposition", aegis_project) ) {
        if (aegis_project == "speciescomposition_pca1") sc_vn = "pca1" 
        if (aegis_project == "speciescomposition_pca2") sc_vn = "pca2" 
        if (aegis_project == "speciescomposition_pca3") sc_vn = "pca3" 
        if (aegis_project == "speciescomposition_ca1")  sc_vn = "ca1" 
        if (aegis_project == "speciescomposition_ca2")  sc_vn = "ca2" 
        if (aegis_project == "speciescomposition_ca3")  sc_vn = "ca3" 
        if (is.null(pL) )  pL = speciescomposition_parameters(  project_class=project_class, variabletomodel=sc_vn, year.assessment=year.assessment   )
        if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
        if (is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
        if ( project_class %in% c("core" ) ) LUT = speciescomposition_db ( p=pL, DS=DS )  
        if ( project_class %in% c( "stmv", "hybrid") )  LUT = aegis_db( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 

          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(sc_vn), na.rm=TRUE) ), sc_vn), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
          }

      }


      if ( grepl("snowcrab", aegis_project) ) {
        if (aegis_project == "snowcrab_number") sc_vn = "number" 
        if (aegis_project == "snowcrab_biomass") sc_vn = "biomass" 
        if (aegis_project == "snowcrab_meansize") sc_vn = "meansize" 
        if (aegis_project == "snowcrab_pa")  sc_vn = "pa" 

        if (is.null(pL) ) {
          if ( sc_vn == "number" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              family="poisson",
              carstm_model_label = "1999_present",  # 1999_present is the default anything else and you are on your own
              selection = list(type = "number")
            )

          } else if ( sc_vn == "biomass" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "1999_present",  # 1999_present is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "biomass")
            )

          } else if ( sc_vn == "meansize" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "1999_present",  # 1999_present is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "meansize")
            )

          } else if ( sc_vn == "pa" ) {
            pL = snowcrab_parameters(
              project_class="carstm",
              yrs=1999:year.assessment,   
              areal_units_type="tesselation",
              carstm_model_label = "1999_present",  # 1999_present is the default anything else and you are on your own
            #   carstm_model_label = paste(   carstm_model_label,   variabletomodel, sep="_")  
              family =  "gaussian" ,  
              selection = list(type = "presence_absence")
            )

          } else { 
            pL = p = bio.snowcrab::load.environment( year.assessment=year.assessment )
          }
        } 

        if (is.null(space_resolution)) if (exists( "pres", pL)) space_resolution = pL$pres
        if (is.null(time_resolution))  if (exists( "tres", pL)) time_resolution =  pL$tres
        
        if ( project_class %in% c("core" ) ) LUT = snowcrab.db ( p=pL, DS=DS )  
        if ( project_class %in% c( "stmv", "hybrid") )  LUT = aegis_db( p=pL, DS="complete" )   
        if ( project_class %in% c("carstm" )) LUT = carstm_model( p=pL, DS="carstm_modelled_summary" ) 

          if ( project_class %in% c("core", "stmv", "hybrid") )  {
            setDT(LUT)
            if ( time_resolution != pL$tres ) {
              LUT$dyear = trunc(LUT$dyear / time_resolution + 1 ) * time_resolution
            }
            if ( space_resolution != pL$pres ) {
              # regrid to another resolution
              LUT$plon = trunc(LUT$plon / space_resolution + 1 ) * space_resolution
              LUT$plat = trunc(LUT$plat / space_resolution + 1 ) * space_resolution
            }
            if ( time_resolution != pL$tres |  space_resolution != pL$pres ) {
              LUT = LUT[, setNames(.(mean( get(sc_vn), na.rm=TRUE) ), sc_vn), by=list(plon, plat, yr, dyear) ]
              LUT$timestamp = lubridate::date_decimal( LUT$yr+LUT$dyear, tz=tz )
            }
            setDF(LUT)
          }

      }

    }

    if (is.null(LUT)) stop( "lookup data not found nor given")

    if (is.null(variable_name)) {
      if ( project_class %in% c("carstm" )) {
        variable_name = LUT$formula_parsed$dependent_variable
      } else {
        variable_name = setdiff( names(LUT), c("plon", "plat", "lon", "lat", "timestamp", "year", "dyear", "yr" ))
      }
    } 
 
    # ------------------

    if (pL$aegis_dimensionality =="space" ) {

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {
        
        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=pL$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=pL$aegis_proj4string_planar_km) # get planar projections of lon/lat in km

        LOCS$tplon = trunc(LOCS$plon / space_resolution + 1 ) * space_resolution
        LOCS$tplat = trunc(LOCS$plat / space_resolution + 1 ) * space_resolution

        plon_range = range( LUT$plon )
        plat_range = range( LUT$plat )
        plons = seq(plon_range[1], plon_range[2], by=space_resolution)
        plats = seq(plat_range[1], plat_range[2], by=space_resolution)
        Sdims = c(length(plons), length(plats))
        Sres = c(space_resolution, space_resolution)
        Sorigin = c( plon_range[1], plat_range[1] )

        ii = match( 
            array_map( "xy->1", LOCS[, c("tplon","tplat")], dims=Sdims, res=Sres, origin=Sorigin ),
            array_map( "xy->1", LUT[,c("plon","plat")], dims=Sdims, res=Sres, origin=Sorigin )
        )

        LOCS$tplon = LOCS$tplat  = NULL
        gc()

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {
            LOCS[[vnm]] = NA  
            LOCS[[vnm]] = LUT[ ii, vnm ]
          } 
        } 

      }

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {

        LOCS0 = NULL
        if (exists("AUID", LOCS)) {
          # this is a special class of lookup taking LUT (usually empirical or aggregated data) and estimating from it for an areal unit (via rasterization)
          LOCS0 = LOCS # store as modifications will be made to LOCS

          # next, get a x-y coordinate for each LOC by rasterizing to LOCS_AU  
          LOCS_AU = st_make_valid(LOCS_AU)
          LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )
          raster_template = raster::raster( LOCS_AU, res=space_resolution, crs=st_crs( pL$aegis_proj4string_planar_km ) )

          # prep-pass with a au_index variable to get index
          # LOCS_AU = st_cast( LOCS_AU, "MULTIPOLYGON")
          # LOCS_AU = st_cast( LOCS_AU, "POLYGON" )
          LOCS_AU$au_index = 1:nrow(LOCS_AU)
          LOCS_AU_raster = fasterize::fasterize( LOCS_AU, raster::raster( LOCS_AU, res=space_resolution, crs=st_crs( LOCS_AU ) ), field="au_index" )  

          # overwrite LOCS with discretized points 
          LOCS = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LOCS_AU_raster)), coords=c("x", "y") )
          st_crs(LOCS) = st_crs( LOCS_AU )
          LOCS$AUID = LOCS_AU$AUID[ match( LOCS[["layer"]], LOCS_AU$au_index )]
        }

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(pL$aegis_proj4string_planar_km) 
        } 

        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT) )  # 

        variable_name = intersect( names(LUT), variable_name )
        LUT = LUT[, variable_name]
        LU_map =  st_points_in_polygons( pts=LUT, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
        if (!exists("AUID", LOCS ))  LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 

        st_geometry(LUT) = NULL
        setDT(LUT)
        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {  
            # LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LUT_regridded = LUT[, setNames(.(mean( get(vnm), na.rm=TRUE) ), vnm), by=.(LU_map) ]
            LOCS[[ vnm ]] = LUT_regridded[ match( LOCS$AUID, LUT_regridded$LU_map ), vnm, with=FALSE ]
          }   
        }
 
        # revert LOCS to original structure and add the vars to it
        if (!is.null(LOCS0)) {
          # summarize LOCS to to original AUID's
          if ( "sf" %in% class(LOCS) ) st_geometry(LOCS) = NULL
          LOCS = as.data.table( LOCS )
          # o = LOCS[,  lapply(.SD, mean, na.rm=TRUE), by=AUID, .SDcols=variable_name]   
          o = LOCS[,  setNames(.(mean( get(variable_name), na.rm=TRUE) ), variable_name), by=AUID ]   
          LOCS0[,variable_name] = o[ match( LOCS0$AUID, o$AUID), .SD, .SDcols=variable_name  ]        
          LOCS = LOCS0
        }

      }

 
      if ( project_class %in% c("carstm" ) & output_format == "points" )  {

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_make_valid(LUT_AU)
        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )

        if (!exists("AUID", LUT_AU)) {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LUT_AU$AUID = as.character(LUT_AU$au_index)
        }
        bm = match( LUT_AU$AUID, LUT$space )  
              
        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(pL$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   
        ii = match( LOCS$AUID, LUT$space[bm] ) 

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
        LUT_AU = st_make_valid(LUT_AU)
        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )

        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster::raster( LUT_AU, res=space_resolution, crs=st_crs( LUT_AU ) ), field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_make_valid(LOCS_AU)
        # LOCS_AU = st_cast( LOCS_AU, "MULTIPOLYGON")
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (! inherits(LOCS, "sf") )   {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
            if (!exists("AUID", LOCS )) LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
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
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ space_index  ]
                }
              }
            }
          } 
        }
      }

    }
 

    ######################################################
 
    if (pL$aegis_dimensionality =="space-year" ) {


      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {

        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=pL$aegis_proj4string_planar_km)
        if (!exists("plon", LOCS)) LOCS = lonlat2planar(LOCS, proj.type=pL$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        if (!exists("dyear", LOCS) | (!exists("yr", LOCS)) ) {
          LOCS$tiyr = lubridate::decimal_date( LOCS$timestamp  ) 
          LOCS$yr = trunc( LOCS$tiyr )
          LOCS$dyear = LOCS$tiyr - LOCS$yr   
        } 

        LOCS$tplon = trunc(LOCS$plon / space_resolution + 1 ) * space_resolution
        LOCS$tplat = trunc(LOCS$plat / space_resolution + 1 ) * space_resolution
        LOCS$tdyear = trunc(LOCS$dyear / time_resolution + 1 ) * time_resolution

        nY = diff( range(LUT$yr) )
        nW = round(1/time_resolution)
        Tdims = c( nY, nW )
        Tres = c(1, time_resolution)
        Torigin = c( min(LUT$yr), 0) 

        plon_range = range( LUT$plon )
        plat_range = range( LUT$plat )
        plons = seq(plon_range[1], plon_range[2], by=space_resolution)
        plats = seq(plat_range[1], plat_range[2], by=space_resolution)
        Sdims = c(length(plons), length(plats))
        Sres = c(space_resolution, space_resolution)
        Sorigin = c( plon_range[1], plat_range[1] )

        ii = match( 
          paste(
            array_map( "xy->1", LOCS[, c("tplon","tplat")], dims=Sdims, res=Sres, origin=Sorigin ), 
            array_map( "ts->1", LOCS[, c("yr", "tdyear")], dims=Tdims, res=Tres, origin=Torigin ), 
            sep="_"
          ), 
          paste( 
            array_map( "xy->1", LUT[,c("plon","plat")], dims=Sdims, res=Sres, origin=Sorigin ), 
            array_map( "ts->1", LUT[,c("yr", "dyear")], dims=Tdims, res=Tres, origin=Torigin ), 
            sep="_" 
          ) 
        )

        LOCS$tplon = LOCS$tplat = LOCS$tdyear = NULL
        gc()

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {
            LOCS[[vnm]] = NA
            LOCS[[vnm]] = LUT[ ii, vnm ]

          } 

        }
    
      }


      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(pL$aegis_proj4string_planar_km) 
        } 

        Tdims = c( diff( range(LUT$yr) ), pL$nw )
        Tres = c(1, pL$tres)
        Torigin = c( min(LUT$yr), 0 ) 

        Sdims = c( pL$nplons, pL$nplats )
        Sres = c( pL$pres, pL$pres )
        Sorigin = pL$origin

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
          array_map( "ts->1", LUT[,c("yr", "dyear")], dims=Tdims, res=Tres, origin=Torigin ), 
          sep="_"
        )

        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = LOCS_AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->1", LOCS[, c("yr", "tdyear")], dims=Tdims, res=Tres, origin=Torigin ), 
          sep="_"
        )
   
        LUT = setDT(st_drop_geometry(LUT))
 
        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {  
            # LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LUT_regridded = LUT[, setNames(.(mean( get(vnm), na.rm=TRUE) ), vnm), by=.(LU_map) ]
            LOCS[[ vnm ]] = LUT_regridded[ match( LOCS$AUID, LUT_regridded$LU_map ), vnm, with=FALSE ]
          }   
        }

 
      }

 


      if ( project_class %in% c("carstm" ) & output_format == "points" )  {
        
        # discretize LUT_AU to LOCS_AU and then re-estimate means by LOCS_AU$AUID then tag each estimate to AUID of LOCS

        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_make_valid(LUT_AU)
        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )
        LUT_AU$au_index = 1:nrow(LUT_AU)
        if (!exists("AUID", LUT_AU)) {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LUT_AU$AUID = as.character(LUT_AU$au_index)
        }
        bm = match( LUT_AU$AUID, LUT$space )  
              
        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(pL$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   
       
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
      
      
        ii = cbind( match( LOCS$AUID, LUT$space[bm] ) )

        jj = cbind( 
          ii, 
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
        LUT_AU = st_make_valid(LUT_AU)
        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")
        LUT_AU = st_cast(LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )
        
        # dims
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster::raster( LUT_AU, res=space_resolution, crs=st_crs( LUT_AU ) ), field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_make_valid(LOCS_AU)
        # LOCS_AU = st_cast( LOCS_AU, "MULTIPOLYGON")
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (! inherits(LOCS, "sf") )   {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
            if (!exists("AUID", LOCS )) LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
          }
        }

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        
        TIMESTAMP_index1 = array_map( "ts->year_index", LOCS[["yr"]], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )
        if (any( TIMESTAMP_index1 < 0)) {
          TIMESTAMP_index1[ which( TIMESTAMP_index1 <= 0 ) ] = NA
        }

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
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[  space_index  ]
                }
              }
              if (kk == 3) {
                ov = o[ pts_AU,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ TIMESTAMP_index1  ]  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index1 ) ]
                }
              }
            }
          } 
        }
      }

    }  # end dimension
    
    ######################################################


    if (pL$aegis_dimensionality =="space-year-season" ) {

      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "points" )  {

        if (!exists("plon", LOCS))  LOCS = lonlat2planar(LOCS, proj.type=pL$aegis_proj4string_planar_km) # get planar projections of lon/lat in km
        if (!exists("plon", LUT))  LUT = lonlat2planar(LUT, proj.type=pL$aegis_proj4string_planar_km)
        
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
    
        if (!exists("dyear", LOCS) | (!exists("yr", LOCS)) ) {
          LOCS$tiyr = lubridate::decimal_date( LOCS$timestamp  ) 
          LOCS$yr = trunc( LOCS$tiyr )
          LOCS$dyear = LOCS$tiyr - LOCS$yr   
        } 

     
        LOCS$tplon = trunc(LOCS$plon / space_resolution + 1 ) * space_resolution
        LOCS$tplat = trunc(LOCS$plat / space_resolution + 1 ) * space_resolution
        LOCS$tdyear = trunc(LOCS$dyear / time_resolution + 1 ) * time_resolution

        nY = diff( range(LUT$yr) )
        nW = round(1/time_resolution)
        Tdims = c( nY, nW )
        Tres = c(1, time_resolution)
        Torigin = c( min(LUT$yr), 0) 

        plon_range = range( LUT$plon )
        plat_range = range( LUT$plat )
        plons = seq(plon_range[1], plon_range[2], by=space_resolution)
        plats = seq(plat_range[1], plat_range[2], by=space_resolution)
        Sdims = c(length(plons), length(plats))
        Sres = c(space_resolution, space_resolution)
        Sorigin = c( plon_range[1], plat_range[1] )

        ii = match( 
          paste(
            array_map( "xy->1", LOCS[, c("tplon","tplat")], dims=Sdims, res=Sres, origin=Sorigin ), 
            array_map( "ts->1", LOCS[, c("yr", "tdyear")], dims=Tdims, res=Tres, origin=Torigin ), 
            sep="_"
          ), 
          paste( 
            array_map( "xy->1", LUT[,c("plon","plat")], dims=Sdims, res=Sres, origin=Sorigin ), 
            array_map( "ts->1", LUT[,c("yr", "dyear")], dims=Tdims, res=Tres, origin=Torigin ), 
            sep="_" 
          ) 
        )

        LOCS$tplon = LOCS$tplat = LOCS$tdyear = NULL
        gc()

        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {
            LOCS[[vnm]] = NA  
            LOCS[[vnm]] = LUT[ ii, vnm ]
          } 
        }



      }

 
      if ( project_class %in% c("core", "stmv", "hybrid") & output_format == "areal_units" )  {

        if (exists("lon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("lon", "lat") )
          st_crs(LUT) = st_crs( projection_proj4string("lonlat_wgs84") )
        } else if (exists("plon", LUT)) {
          LUT = sf::st_as_sf( LUT, coords=c("plon", "plat") )
          st_crs(LUT) = st_crs(pL$aegis_proj4string_planar_km) 
        } 
    
        Tdims = c( diff( range(LUT$yr) ), pL$nw )
        Tres = c(1, pL$tres)
        Torigin = c( min(LUT$yr), 0 ) 

        Sdims = c( pL$nplons, pL$nplats )
        Sres = c( pL$pres, pL$pres )
        Sorigin = pL$origin

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
          array_map( "ts->1", st_drop_geometry( LUT) [,c("yr", "dyear")], dims=Tdims, res=Tres, origin=Torigin), 
          sep="_"
        )
 
        LOCS_map =  paste( 
          st_points_in_polygons( pts=LOCS, polys = LOCS_AU[, "AUID"], varname= "AUID" ),  
          array_map( "ts->1", st_drop_geometry(LOCS)[ , c("yr", "dyear") ], dims=Tdims, res=Tres, origin=Torigin), 
          sep="_"
        )

 
        LUT = setDT(st_drop_geometry(LUT))
 
        for (vnm in variable_name) {
          if ( vnm %in% names(LUT )) {  
            # LUT_regridded = tapply( st_drop_geometry(LUT)[, vnm], LU_map, FUN=FUNC, na.rm=TRUE )
            LUT_regridded = LUT[, setNames(.(mean( get(vnm), na.rm=TRUE) ), vnm), by=.(LU_map) ]
            LOCS[[ vnm ]] = LUT_regridded[ match( LOCS$AUID, LUT_regridded$LU_map ), vnm, with=FALSE ]
          }   
        }

 
      }



      if ( project_class %in% c("carstm" ) & output_format == "points" )  {
        # areal unit (LUT) to points (LOCS)
        nw = length(LUT$cyclic)
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))

        if (is.null(LUT_AU)) LUT_AU = LUT$sppoly
        LUT_AU = st_make_valid(LUT_AU)

        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")  # causes additional polys ..
        LUT_AU = st_cast( LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )
        LUT_AU$au_index = 1:nrow(LUT_AU)
        if (!exists("AUID", LUT_AU)) {
          message ("AUID not found in LOCS_AU polygons, setting AUID as row number of polygons")
          LUT_AU$AUID = as.character(LUT_AU$au_index)
        }
        bm = match( LUT_AU$AUID, LUT$space )  


        if (exists("lon", LOCS)) {
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs( projection_proj4string("lonlat_wgs84") )
          LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
        } else if  (exists("plon", LOCS)) { 
          LOCS = sf::st_as_sf( LOCS, coords=c("lon", "lat") )
          st_crs(LOCS) = st_crs(pL$aegis_proj4string_planar_km)
        }

        LOCS$AUID = st_points_in_polygons( pts=LOCS, polys = LUT_AU[, "AUID"], varname= "AUID" )   

        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
      
  
        ii = cbind( match( LOCS$AUID, LUT$space[bm] ) )
   
        jj = cbind( 
          ii, 
          array_map( "ts->year_index", LOCS[["yr"]], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) ) 
        )

  
        LOCS_DF = LOCS
        if (inherits(LOCS_DF, "sf")) LOCS_DF = st_drop_geometry(LOCS_DF)

        ll = cbind( 
          ii, 
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
        LUT_AU = st_make_valid(LUT_AU)
        # LUT_AU = st_cast( LUT_AU, "MULTIPOLYGON")
        LUT_AU = st_cast (LUT_AU, "POLYGON" )
        LUT_AU = st_make_valid(LUT_AU)
        LUT_AU = sf::st_transform( LUT_AU, crs=st_crs(pL$aegis_proj4string_planar_km) )

        nw = length(LUT$cyclic)
        ny = length(LUT$time)
        yr0 = min(as.numeric(LUT$time))
 
        if (!exists("AUID", LUT_AU)) LUT_AU$AUID = as.character(1:nrow(LUT_AU))
        bm = match( LUT_AU$AUID, LUT$space )  # should not be required but just in case things are subsetted

        # lut_uid is A LOCAL index of LUT_AU /LUT .. rasterize it
        LUT_AU$lut_uid = 1:nrow(LUT_AU)
        raster_template = raster::raster( LUT_AU, res=space_resolution, crs=st_crs( LUT_AU ) )
        
        LUT_AU_raster = fasterize::fasterize( LUT_AU, raster_template, field="lut_uid" )  
        LUT_AU_pts = sf::st_as_sf( as.data.frame( raster::rasterToPoints(LUT_AU_raster)), coords=c("x", "y") )
        st_crs(LUT_AU_pts) = st_crs( LUT_AU ) 

        # format output polygon structure 
        LOCS_AU = sf::st_transform( LOCS_AU, crs=st_crs(LUT_AU) )  # LOCS_AU .... must be sent ... <---------
        LOCS_AU = st_make_valid(LOCS_AU)
        # LOCS_AU = st_cast( LOCS_AU, "MULTIPOLYGON")
        LOCS_AU = st_cast( LOCS_AU, "POLYGON")
        if (!exists("AUID", LOCS_AU))  {
          message ("AUID not found in the LOCS_AU polygons, setting AUID as row number of polygons")
          LOCS_AU$AUID = as.character(1:nrow(LOCS_AU))
        }

        if (! inherits(LOCS, "sf") )   {
          if (exists("lon", LOCS )) {
            LOCS = st_as_sf( LOCS, coords=c("lon","lat"), crs=st_crs(projection_proj4string("lonlat_wgs84")) )
            LOCS = sf::st_transform( LOCS, crs=st_crs(pL$aegis_proj4string_planar_km) )
            if (!exists("AUID", LOCS )) LOCS[["AUID"]]  = st_points_in_polygons( pts=LOCS, polys=LOCS_AU[, "AUID"], varname= "AUID" ) 
          }
        }
        
        if (! inherits(LOCS$timestamp, "POSIXct") )  LOCS$timestamp =  lubridate::date_decimal( LOCS$timestamp, tz=tz )
        if (! exists("yr", LOCS) ) LOCS$yr = lubridate::year(LOCS$timestamp) 
        if (! exists("dyear", LOCS) ) LOCS$dyear = lubridate::decimal_date( LOCS$timestamp ) - LOCS$yr
    
        LOCS_DF = LOCS
        if (inherits(LOCS_DF, "sf")) LOCS_DF = st_drop_geometry(LOCS_DF)

        TIMESTAMP_index1 = array_map( "ts->year_index", LOCS_DF[, "yr"], dims=c(ny ), res=c( 1  ), origin=c( yr0 ) )
        TIMESTAMP_index2 = array_map( "ts->2", LOCS_DF[, c("yr", "dyear")], dims=c(ny, nw), res=c( 1, 1/nw ), origin=c( yr0, 0) )
        
        if (any( TIMESTAMP_index1 < 0)) {
          TIMESTAMP_index1[ which( TIMESTAMP_index1 <= 0 ) ] = NA
        }
        if (any( TIMESTAMP_index2 < 0)) {
          warning ("cyclic time index is negative: lookup table does not contain data to lookup ... \n 
            lookup data needs to be expanded or time periods reduced in model")
          TIMESTAMP_index2[ which( TIMESTAMP_index2 <= 0 ) ] = NA
        }

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
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[  space_index  ]
                }
              }
              if (kk == 3) {
                ov = o[ pts_AU,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ TIMESTAMP_index1  ]  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index1 ) ]
                }
              }
              if (kk == 4) {
                ov = o[ pts_AU,,, which(dimnames(o)$stat == stat_var)  ] 
                LUT_as_LOCS_AU = apply( ov, MARGIN=c(2,3), FUN=aggFUN )
                space_index = match( as.character(LOCS$AUID), as.character(dimnames(LUT_as_LOCS_AU )[[1]] )  )   # AUID of LOCS_AU (from LUT_AU_pts_AUID)
                if (all(is.na(space_index))) {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ TIMESTAMP_index2  ]  # fiddling required when only 1 AU
                } else {
                  LOCS[,vnm] = LUT_as_LOCS_AU[ cbind( space_index, TIMESTAMP_index2 ) ]
                }
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
        if (project_class=="carstm") {
          variable_name = paste( paste0(variable_name[[1]],  collapse="_"),  statvars[1], sep="_" )
        }          
        if (exists( variable_name, LOCS)) {
          LOCS = LOCS[[variable_name]] 
        } else {
          LOCS = rep(NA, nrow(LOCS))
        }
      }  
    }

    return(LOCS)

}
