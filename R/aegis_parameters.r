

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
    # aegis defaults .. for use in stmv
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

    # generic models forms & var names, common to most aegis projects

    if (!exists("Y", p$stmv_variables)) p$stmv_variables$Y = "not_defined" # this can be called to get covars.. do not stop

    if (!exists("LOCS", p$stmv_variables)) p$stmv_variables$LOCS = c("plon", "plat")

    if ( exists("stmv_global_modelengine", p)) {
      if ( p$stmv_global_modelengine != "none" ) {
        if ( !exists("stmv_global_modelformula", p)) {
          if (!exists("stmv_local_modelformula", p)) p$stmv_global_modelformula = formula( paste(
            p$stmv_variables$Y, '~ s(t, k=3, bs="ts") + s( tsd, k = 3, bs = "ts") ',
            ' + s( tmin, k = 3, bs = "ts") + s( tmax, k = 3, bs = "ts") + s( degreedays, k = 3, bs = "ts")  ',
            ' + s(log(z), k=3, bs="ts") + s( log(dZ), k=3, bs="ts") + s( log(ddZ), k=3, bs="ts")',
            ' + s(log(substrate.grainsize), k=3, bs="ts") ' ))
          }
      }
    }

    # local model options
    if ( exists("stmv_local_modelengine", p)) {
      if ( p$stmv_local_modelengine != "none" ) {
        if (!exists("stmv_local_modelformula", p)) {

          if (p$stmv_local_modelengine =="twostep") {

            # p$stmv_local_modelformula = formula( paste(
            #   p$stmv_variables$Y, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
            #   ' + s(cos.w, sin.w, yr, bs="ts", k=16) ',
            #   ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=16, bs="ts") ' ) )  # required for the local TS modelling
            if (p$stmv_twostep_space == "gam") {
              if (!exists("stmv_local_modelformula_space", p)) p$stmv_local_modelformula_space = formula( paste(
              p$stmv_variables$Y, '~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
            }

            if (p$stmv_twostep_time == "gam") {
              if (!exists("stmv_local_modelformula_time", p)) p$stmv_local_modelformula_time = formula( paste(
                p$stmv_variables$Y, '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
                  ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(log(z), k=3, bs="ts") ' ,
                  ' + s(cos.w, sin.w, yr, bs="ts", k=30) + s(log(z), plon, plat, k=30, bs="ts") '
                ) )  # required for the local TS modelling
              }

          }  else if (p$stmv_local_modelengine == "gam") {
            if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
              p$stmv_variables$Y, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
              ' + s(cos.w, sin.w, yr, bs="ts", k=16) ',
              ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=16, bs="ts") ' ) )
              # similar to GAM model .. this is the seed model
          }  else if (p$stmv_local_modelengine == "bayesx") {
            # bayesx families are specified as characters, this forces it to pass as is and
            # then the next does the transformation internal to the "stmv__bayesx"
            # alternative models .. testing .. problem is that SE of fit is not accessible?
            if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
              p$stmv_variables$Y, ' ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps") +s(z, bs="ps")',
              ' + sx(plon, bs="ps") + sx(plat,  bs="ps")',
              ' + sx(plon, plat, cos.w, sin.w, yr, bs="te") ' )
              # te is tensor spline
            )
          } else {
            # message( "The specified stmv_local_modelengine is not tested/supported ... you are on your own ;) ..." )
          }
        }
      }
    }


    return(p)
  }



}
