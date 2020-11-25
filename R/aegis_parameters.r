

aegis_parameters = function( p=NULL, DS=NULL, ... ) {

  ## these parameters are here in an accessible format such that they can be used by other projects as aegis lookups ..
  ## consider moving bathymetry, temperature and substrate parameters here top to make things all centralized

  # ---------------------
  p = parameters_add(p, list(...)) # add passed args to parameter list, priority to args


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

    p = parameters_add_without_overwriting( p,
      storage_backend="bigmemory.ram",
      clusters = rep("localhost", detectCores() ),
      boundary = FALSE,
      stmv_filter_depth_m = 0, # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # .. no space or time here .. only in the local model
      stmv_global_modelengine ="none", #default
      stmv_global_family = gaussian(link="identity"),
    ## local model-specific options
      stmv_local_modelengine ="none",
      stmv_rsquared_threshold = 0.75, # lower threshold
      stmv_distance_statsgrid = 5, # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction  = p$stmv_distance_statsgrid *0.75 # this is a half window km
      stmv_distance_scale = c(25, 30, 40), # km ... approx guess of 95% AC range
      stmv_nmin = 30, # min number of data points req before attempting to model timeseries in a localized space
      stmv_nmax = 6000, # no real upper bound
      data_sources = c("groundfish", "snowcrab")
    )

    # lookup temporal params for the SSE domain
    # obtain current temperature years

    p = aegis_stmv_modelformula(p=p)  # use generic models if none are specified
    p = stmv_variablelist(p=p)  # decompose into covariates from formulas , etc

    return(p)
  }



}
