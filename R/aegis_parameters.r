

aegis_parameters = function( p=NULL, DS=NULL, ... ) {

  ## these parameters are here in an accessible format such that they can be used by other projects as aegis lookups ..
  ## consider moving bathymetry, temperature and substrate parameters here top to make things all centralized

  # ---------------------
  p = parameters_control(p, list(...), control="add") # add passed args to parameter list, priority to args


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
  if ( DS=="stmv" ) {
    # aegis defaults ..
    p$libs =  RLibrary( unique( c( p$libs, "stmv" )  ) )

    if (!exists("storage_backend", p)) p$storage_backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )
    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("stmv_filter_depth_m", p)) p$stmv_filter_depth_m = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # .. no space or time here .. only in the local model
    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine ="none" #default
    if (!exists("stmv_global_family", p)) p$stmv_global_family = gaussian(link="identity")

    ## local model-specific options
    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine ="none"

    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.75 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction  = p$stmv_distance_statsgrid *0.75 # this is a half window km
    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(25, 30, 40) # km ... approx guess of 95% AC range
    if (!exists("stmv_nmin", p)) p$stmv_nmin = 30 # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = 6000 # no real upper bound

    if (!exists("Y", p$stmv_variables)) p$stmv_variables$Y = "not_defined" # this can be called to get covars.. do not stop

    # lookup temporal params for the SSE domain
    if (!exists("data_sources", p)) p$data_sources = c("groundfish", "snowcrab")
    # obtain current temperature years

    p = aegis_stmv_modelformula(p=p)  # use generic models if none are specified
    p = stmv_variablelist(p=p)  # decompose into covariates from formulas , etc

    return(p)
  }



}
