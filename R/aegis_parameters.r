

aegis_parameters = function( p=NULL, DS=NULL, ... ) {

  ## these parameters are here in an accessible format such that they can be used by other projects as aegis lookups ..
  ## consider moving bathymetry, temperature and substrate parameters here top to make things all centralized

  # ---------------------
  # deal with additional passed parameters
  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ))
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable


  # ---------------------
  # create/update library list
  p$libs = c( p$libs, RLibrary ( "colorspace",  "fields", "geosphere", "lubridate",  "lattice",
    "maps", "mapdata", "maptools", "parallel",  "rgdal", "rgeos",  "sp", "splancs", "GADMTools" ) )
  p$libs = unique( c( p$libs, project.library ( "aegis") ) )




  # ---------------------
  # lookup parameters
  if (DS %in% c("speciescomposition", "speciesarea", "metabolism", "condition", "sizespectrum") ) {
    res = switch( DS,
      speciescomposition = aegis.speciescomposition::speciescomposition_parameters(p=p, ...),
      speciesarea = aegis.speciesarea::speciesarea_parameters(p=p, ...),
      metabolism = aegis.metabolism::metabolism_parameters(p=p, ...),
      condition = aegis.condition::condition_parameters(p=p, ...),
      sizespectrum = aegis.sizespectrum::sizespectrum_parameters(p=p, ...)
    )
    return(res)
  }



  # ----------------------------------
  if ( DS=="carstm" ) {
    if ( !exists("project_name", p)) stop("project_name is required")

    if ( !exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project_name )
    if ( !exists("datadir", p) )   p$datadir  = file.path( p$data_root, "data" )
    if ( !exists("modeldir", p) )  p$modeldir = file.path( p$data_root, "modelled" )

    if (!exists("areal_units_strata_type", p )) p$areal_units_strata_type = "lattice" #
    if (!exists("areal_units_constraint", p )) p$areal_units_constraint = "none" #
    if (!exists("areal_units_overlay", p )) p$areal_units_overlay = "none" #
    if (!exists("areal_units_resolution_km", p )) stop( "areal_units_resolution_km should be defined ... " ) # km
    if (!exists("areal_units_proj4string_planar_km", p )) stop( "areal_units_proj4string_planar_km should be defined ... " ) # km
    if (!exists("timeperiod", p) )  p$timeperiod="default"
    if (!exists("inputdata_spatial_discretization_planar_km", p) )  p$inputdata_spatial_discretization_planar_km = 1

    if (!exists("auid", p) ) p$auid = paste(
      p$spatial_domain,
      paste0(p$areal_units_overlay, collapse="_"),
      p$areal_units_resolution_km,
      p$areal_units_strata_type,
      p$areal_units_constraint,
      p$timeperiod,
      sep="_"
    )

    if (!exists("discretization", p) ) p$discretization = list()
    if (!exists("z", p$discretization) ) p$discretization$z = c(1.25, 2.5, 5, 10, 20, 40, 80, 160, 320, 640, 1280 )  # depth cut points
    # if (!exists("z", p$discretization) ) p$discretization$z = c(2.5, 5, 10, 20, 40, 80, 160, 320, 640 )
    if (!exists("dz", p$discretization) ) p$discretization$dz = c(0.01, 0.1,  1, 2, 4, 6, 8, 10, 12 )  # slope cut points
    if (!exists("ddz", p$discretization) ) p$discretization$ddz = c(0.01, 0.1, 0.2, 0.4, 0.8, 1, 2, 4  )  # slope cut points
    if (!exists("substrate.grainsize", p$discretization) ) p$discretization$substrate.grainsize = c( 0, 0.1, 0.2, 0.4, 0.6, 0.8, 1.0, 1.2, 1.6, 2.0, 4.0, 8.0 )

    if (!exists("t", p$discretization) ) p$discretization$t = c( -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20 )
    if (!exists("tsd", p$discretization) ) p$discretization$tsd = c( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15, 20 )
    if (!exists("tmin", p$discretization) ) p$discretization$tmin = c( -2, 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10  )
    if (!exists("tmax", p$discretization) ) p$discretization$tmax = c( 0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 15 )
    if (!exists("degreedays", p$discretization) ) p$discretization$degreedays = c(10, 100, 200, 400, 800, 1000, 1500, 2000, 2500, 3000, 3500, 4000, 5000)


    return(p)
  }


  # ----------------------------------
  if ( DS=="stmv_spatial_model" ) {
    # generic stmv settings space AND time interp (and not just spatial)

    p$libs = RLibrary( unique( c( p$libs, "stmv" ) ) )

    if (!exists("storage_backend", p)) p$storage_backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )
    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("stmv_filter_depth_m", p)) p$stmv_filter_depth_m = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.75 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction  = p$stmv_distance_statsgrid *0.75 # this is a half window km
    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(25, 30, 40) # km ... approx guess of 95% AC range
    if (!exists("stmv_nmin", p)) p$stmv_nmin = 200 # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = 8000 # no real upper bound


    if (!exists("Y", p$variables)) p$variables$Y = "not_defined" # this can be called to get covars.. do not stop

    p = aegis_stmv_modelformula(p=p)  # use generic models if none are specified
    p = stmv_variablelist(p=p)  # decompose into covariates from formulas , etc

    return(p)
  }


  # ----------------------------------
  if ( DS=="stmv_spatiotemporal_model" ) {
    # generic stmv settings space AND time interp (and not just spatial), mostly for aegis "indicators"

    p$libs =  RLibrary( unique( c( p$libs, "stmv" )  ) )

    if (!exists("storage_backend", p)) p$storage_backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )
    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("stmv_filter_depth_m", p)) p$stmv_filter_depth_m = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)

    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.2 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 2 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction = p$stmv_distance_statsgrid *0.75 # this is a half window km
    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(50, 60, 75, 100) # km ... approx guess of 95% AC range .. data tends to be sprse realtive to pure space models

    # lookup temporal params for the SSE domain
    if (!exists("data_sources", p)) p$data_sources = c("groundfish", "snowcrab")
    # obtain current temperature years

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)

    if (!exists("stmv_nmin", p)) p$stmv_nmin = floor( 7 * p$ny ) # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = max( floor( 7 * p$ny ) * 11, 8000) # no real upper bound

    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = {c(1:p$nw)-1} / p$nw # intervals of decimal years... fractional year breaks
    p$dyear_centre = p$dyears[ round(p$nw/2) ] + p$tres/2

    if (!exists("prediction_dyear", p)) p$prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()

    # must specify, else assumed = 1 (1= no time)
    if (!exists("stmv_dimensionality", p)) p$stmv_dimensionality = "space-year"  # default

    if (p$stmv_dimensionality == "space-year") {
      ## nt=ny annual time steps (default)
      if (!exists("nt", p)) p$nt = p$ny   # ie, default is an annual model (no p$nw)
      # output timeslices for predictions in decimla years, yes all of them here
      if (!exists("prediction_ts", p)) p$prediction_ts = p$yrs + p$prediction_dyear
    } else if (p$stmv_dimensionality == "space-year-season") {
      ## nt = ny*nw is seassonal

      if (!exists("nt", p)) p$nt = p$nw*p$ny # i.e., seasonal with p$nw (default is annual: nt=ny)
      tout = expand.grid( yr=p$yrs, dyear=1:p$nw, KEEP.OUT.ATTRS=FALSE )
      tout$tiyr = tout$yr + tout$dyear/p$nw - p$tres/2 # mid-points
      tout = tout[ order(tout$tiyr), ]
      # output timeslices for predictions in decimla years, yes all of them here
      if (!exists("prediction_ts", p)) p$prediction_ts = tout$tiyr   # predictions at these time values (decimal-year)
    }

    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # .. no space or time here .. only in the local model
    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine ="none" #default
    if (!exists("stmv_global_family", p)) p$stmv_global_family = gaussian(link="identity")

    ## local model-specific options
    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine ="none"

    if (!exists("Y", p$variables)) p$variables$Y = "not_defined" # this can be called to get covars.. do not stop


    return(p)
  }

}
