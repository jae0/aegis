

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
  p$libs = c( p$libs, project.library ( "aegis.base") )


  # ---------------------

  if (DS=="substrate") {

    if (!exists("project.name", p) ) p$project.name = "substrate"
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("spatial.domain", p) ) p$spatial.domain = "canada.east.highres"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "canada.east", "SSE", "snowcrab", "SSE.mpa" )
    p = spatial_parameters( p=p)  # default (= only supported resolution of 0.2 km discretization)  .. do NOT change

    if (!exists("DATA", p) ) p$DATA = 'substrate.db( p=p, DS="stmv.inputs" )'

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")

    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine="fft"  # currently the perferred approach

    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine = "gam"
    if (!exists("stmv_global_modelformula", p)) p$stmv_global_modelformula = formula( paste(
      'substrate.grainsize ~  s( log(z), k=3, bs="ts") + s( log(dZ), k=3, bs="ts" ) + s( log(ddZ), k=3, bs="ts" ) ') )
    if (!exists("stmv_global_family", p)) p$stmv_global_family = gaussian(link="log")

    if (p$stmv_global_modelengine == "gam") {
      p$libs = unique( c( p$libs, RLibrary ("mgcv")) )
      if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer = c("outer", "bfgs")
      if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
        'substrate.grainsize ~ s(plon,k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=200, bs="ts")' ) )
    }

    # some tweaked options for substrate
    if ( p$stmv_local_modelengine %in% c("krige" )) {
      # nothing to do ... faster than gstat
    }

    if (p$stmv_local_modelengine == "gam") {
      p$libs = unique( c( p$libs, RLibrary ("mgcv")))
      if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE
      if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer = c("outer", "bfgs")
    }

    if ( p$stmv_local_modelengine == "bayesx" ) {
      p$libs = unique( c( p$libs, RLibrary ("bayesx")) )
      if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
        'substrate.grainsize ~ s(plon, bs="ps") + s(plat, bs="ps") + s(plon, plat, bs="te")') )  # more detail than "gs" .. "te" is preferred
      if (!exists("stmv_local_model_bayesxmethod", p)) p$stmv_local_model_bayesxmethod="MCMC"  # REML actually seems to be the same speed ... i.e., most of the time is spent in thhe prediction step ..
      if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE

    }

    if ( p$stmv_local_modelengine == "fft" ) {
      # definitely a cleaner (not overly smoothed) image than a GAM
      # NOTE that  p$stmv_lowpass_phi and  p$stmv_lowpass_nu are very critical choices
      # if (!exists("stmv_fft_filter", p)) p$stmv_fft_filter = "lowpass" # only act as a low pass filter .. depth has enough data for this. Otherwise, use:
      # if (!exists("stmv_fft_filter", p)) p$stmv_fft_filter = "matern" to ~ krige
      if (!exists("stmv_lowpass_phi", p)) p$stmv_lowpass_phi = p$pres*2 # FFT based method when operating gloablly
      if (!exists("stmv_lowpass_nu", p)) p$stmv_lowpass_nu = 0.5 # this is exponential covar
    }

    if (!exists("stmv_dimensionality", p)) p$stmv_dimensionality="space"

    p = aegis_parameters( p=p, DS="stmv_spatial_model"  )
    return(p)
  }

  # ---------------------

  if (DS=="temperature") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )

    # define focal years
    if (!exists( "yrs", p)) p$yrs = 1950:lubridate::year(lubridate::now())  # default

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"

    if ( !exists("spatial.domain", p) ) p$spatial.domain = "canada.east" # canada.east.highres and canada.east.superhighres result in memory overflow
    if ( !exists("spatial.domain.subareas", p) )  p$spatial.domain.subareas = c( "SSE.mpa", "SSE", "snowcrab" ) # target domains and resolution for additional data subsets .. add here your are of interest

    p = spatial_parameters( p=p )  # default grid and resolution

    if ( !exists("pres_discretization_temperature", p) )  p$pres_discretization_temperature = p$pres / 100 # controls resolution of data prior to modelling (km .. ie 100 linear units smaller than the final discretization pres)
    if ( !exists("DATA", p) ) p$DATA = 'temperature.db( p=p, DS="stmv.inputs" )'

    if ( !exists("additional.data", p) )  p$additional.data=c("groundfish", "snowcrab", "USSurvey_NEFSC", "lobster")

    if ( !exists("bstats", p) )  p$bstats = c("tmean", "tsd", "tmin", "tmax", "tamplitude", "degreedays" )

    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # marginally useful .. consider removing it.
    if (!exists("stmv_global_modelformula", p)) p$stmv_global_modelformula = formula( t ~ s(z, bs="ts" + s(s.range, bs="ts") + s(dZ, bs="ts") + s(ddZ, bs="ts") + s(log(substrate.grainsize), bs="ts")  ) )
    if (!exists("stmv_global_family", p))  p$stmv_global_family = gaussian()
    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine = "glm"

    # local model options
    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine = "gam" # gam is the most flexible
    if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE

    if (p$stmv_local_modelengine =="gam") {
      if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer=c("outer", "bfgs")
      if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
        't ~ s(yr, k=25, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
          '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
          '+ s(log(z), plon, plat, k=25, bs="ts") + s( cos.w, sin.w, yr, k=25, bs="ts")') )
        # more than 100 knots and it takes a very long time, 50 seems sufficient, given the large-scaled pattern outside of the prediction box
        # other possibilities:
        #     seasonal.basic = ' s(yr) + s(dyear, bs="cc") ',
        #     seasonal.smoothed = ' s(yr, dyear) + s(yr) + s(dyear, bs="cc")  ',
        #     seasonal.smoothed.depth.lonlat = ' s(yr, dyear) + s(yr, k=3) + s(dyear, bs="cc") +s(z) +s(plon) +s(plat) + s(plon, plat, by=yr), s(plon, plat, k=10, by=dyear ) ',

    }

    if (p$stmv_local_modelengine =="fft") {
      # no need for formulas, etc
      # lowpass seems a bit too noisy
      # matern and lowpass_matern are over-smooth
      # p$stmv_fft_filter = "lowpass" # only act as a low pass filter .. depth has enough data for this. Otherwise, use:
      # p$stmv_fft_filter = "matern" # to ~ unviersal krige with external drift
      # for fft-based methods that require lowpass:
      if (!exists("stmv_lowpass_phi", p))  p$stmv_lowpass_phi = p$pres / 5 # FFT-baed methods cov range parameter .. not required for "matern" ..
      if (!exists("stmv_lowpass_nu", p))  p$stmv_lowpass_nu = 0.5
    }

    if (p$stmv_local_modelengine == "gaussianprocess2Dt") {
      message( "NOTE:: The gaussianprocess2Dt method is really slow .. " )
    }

    if (p$stmv_local_modelengine =="twostep") {
      # similar to GAM model but no spatial interpolation .. space is handled via simple FFT-based kriging
      # if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
      #   't ~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
      #     '+ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") ',
      #     '+ s(log(z), plon, plat, cos.w, sin.w, yr, k=100, bs="ts")') )

      if (!exists("stmv_twostep_time", p)) p$stmv_twostep_time = "gam"
      if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "fft" #  matern, krige (very slow), lowpass, lowpass_matern

      if (p$stmv_twostep_time == "gam")  {
        if (!exists("stmv_local_modelformula_time", p))  p$stmv_local_modelformula_time = formula( paste(
           't',  '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts")  ',
           '+ s( log(z), k=3, bs="ts") + s( plon, k=3, bs="ts") + s( plat, k=3, bs="ts")  ',
           '+ s( cos.w, sin.w, yr, k=15, bs="ts") + s( log(z), plon, plat, k=15, bs="ts")  '
          ) )
      }

      if (p$stmv_twostep_space == "gam")  {
        # very resource intensive ..
        if (!exists("stmv_local_modelformula_space", p))  p$stmv_local_modelformula_space = formula( paste(
        't ~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
      }

      if (p$stmv_twostep_space == "fft" ) {
        if (!exists("stmv_fft_filter", p)) p$stmv_fft_filter="matern"  #  matern, krige (very slow), lowpass, lowpass_matern
        # for fft-based methods that require lowpass:
        if (!exists("stmv_lowpass_phi", p))  p$stmv_lowpass_phi = p$pres / 5 # FFT-baed methods cov range parameter .. not required for "matern" ..
        if (!exists("stmv_lowpass_nu", p))  p$stmv_lowpass_nu = 0.5
      }

    }

    if (p$stmv_local_modelengine == "bayesx") {
      # bayesx families are specified as characters, this forces it to pass as is and
      # then the next does the transformation internal to the "stmv__bayesx"
      if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
        't ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps")',
          ' + sx(plon, bs="ps") + sx(plat, bs="ps") ',
          ' + sx(plon, plat, cos.w, sin.w, yr, bs="te")'  # te is tensor spline
      ))
      if (!exists("stmv_local_model_bayesxmethod", p))  p$stmv_local_model_bayesxmethod="MCMC"
      if (!exists("stmv_local_model_distanceweighted", p))  p$stmv_local_model_distanceweighted = FALSE
    }

    if (!exists("stmv_global_modelformula", p)) p$stmv_global_modelformula = formula( paste( 'z ~ 1') )

    if (!exists("stmv_dimensionality", p)) p$stmv_dimensionality="space-year-season"

    p = aegis_parameters( p=p, DS="stmv_spatiotemporal_model" )

    # intervals of decimal years... fractional year breaks finer than the default 10 units (taking daily for now..)
    #.. need to close right side for "cut" .. controls resolution of data prior to modelling
    if (!exists("dyear_discretization_rawdata", p)) p$dyear_discretization_rawdata = c( {c(1:365)-1}/365, 1)

    return(p)

  }


  # ---------------------

  if (DS=="lookuptables") {
    if (!exists("yrs", p)) p$yrs = 1950:lubridate::year(lubridate::now())  # default --- not all years have data .. where missing they will be filled with global means
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )
    p = spatial_parameters( p=p)  # default (= only supported resolution of 0.2 km discretization)  .. do NOT change
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }



  # ---------------------

  if (DS=="biochem") {
    if (!exists("project.name", p) ) p$project.name=DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    # define focal years
    if (!exists( "yrs", p)) p$yrs = 1950:lubridate::year(lubridate::now())  # default

    p$DATA = 'biochem.db( p=p, DS="stmv_inputs" )'
    p$varstomodel = c( )
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )
    p = spatial_parameters( p=p)
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="survey"){
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    p$DATA = 'survey.db( p=p, DS="stmv_inputs" )'
    p$taxa =  "maxresolved"
    p$varstomodel = c()
    p$libs = c( p$libs,  project.library( "netmensuration" ))
    p$taxa.of.interest = aegis::groundfish.variablelist("catch.summary")
    p$season = "summer"
    p$taxa =  "maxresolved"
    p$nw = 10  # from temperature.r, number of intervals in a year
    p$clusters = rep("localhost", detectCores() )
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )
    p = spatial_parameters( p=p)
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model",stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="landings"){
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    p$DATA = 'landings.db( p=p, DS="stmv_inputs" )'
    p$marfis.years=2002:lubridate::year(lubridate::now())
    p$varstomodel = c()
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )
    p = spatial_parameters( p=p)
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
   }


  # ---------------------
  if (DS=="speciescomposition") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # NOTE:: this is short as groundfish species id is inconsistent

    p$DATA = 'aegis_db( p=p, DS="stmv_inputs" )'
    p$taxa = "maxresolved"
    if (!exists("varstomodel", p) ) p$varstomodel = c( "pca1", "pca2", "ca1", "ca2" )
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )

    p = spatial_parameters( p=p)
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }

  # ---------------------

  if (DS=="condition") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1970:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$DATA = 'aegis_db( p=p, DS="stmv_inputs" )'

    p$varstomodel = c( "coAll", "coFish", "coElasmo", "coGadoid", "coDemersal", "coPelagic",
                       "coSmallPelagic", "coLargePelagic", "coSmallDemersal",   "coLargeDemersal" )

    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )

    p = spatial_parameters( p=p)
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="metabolism") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1970:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$DATA = 'aegis_db( p=p, DS="stmv_inputs" )'

    p$taxa = "alltaxa"   # do not use any other category
    p$varstomodel = c( "mr", "smr", "Pr.Reaction" , "Ea", "A", "zn", "zm", "qn", "qm", "mass", "len"  )

    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )

    p = spatial_parameters( p=p)

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="sizespectrum") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1970:lubridate::year(lubridate::now()))  # years for modelling and interpolation

    p$DATA = 'aegis_db( p=p, DS="stmv_inputs" )'
    p$libs = c( p$libs, RLibrary ( "bigmemory" ) )
    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    p$use.bigmemory.file.backing = FALSE  # for data assimilation, p$use.bigmemory.file.backing = TRUE  # file-backing is slower but can use all cpu's in a distributed cluster

    p$taxa = "maxresolved"
    # for spatial interpolation of nss stats
    # p$varstomodel = c( "nss.rsquared", "nss.df", "nss.b0", "nss.b1", "nss.shannon" )
    p$varstomodel = c( "nss.b0", "nss.b1", "nss.shannon" )
    # for generation of nss

    p$nss.distances=50  # km
    p$nss.stimes= 50 # days
    p$nss.type ="mass"
    p$nss.base =2
    p$nss.taxa = "all"
    if (p$nss.type=="mass") p$nss.bins = bins.df( "gf.mass", p$nss.base )
    if (p$nss.type=="len")  p$nss.bins = bins.df( "gf.len",  p$nss.base )

    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )

    p = spatial_parameters( p=p)

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="speciesarea") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation

    p$DATA = 'aegis_db( p=p, DS="stmv_inputs" )'
    p$libs = unique( c( p$libs, RLibrary ( "bigmemory" ) ) )
    p$varstomodel = c( "C", "Z", "T", "Npred" )
    # faster to use RAM-based data objects but this forces use only of local cpu's
    # configure SHM (shared RAM memory to be >18 GB .. in fstab .. in windows not sure how to do this?)
    # file-backing is slower but can use all cpu's in a distributed cluster
    p$use.bigmemory.file.backing = FALSE
    p$speciesarea.method = "glm"
    p$pred.radius = 50 # km
    p$timescale = c( 0, 1, 2 ) # yr
    p$lengthscale = c( 10, 15, 20, 25, 30, 35, 40, 45, 50, 60, 70, 80, 90, 100, 110, 120 )  # km used in counting for rarefaction curve
    p$taxa = "maxresolved" # p$taxa = "family.or.genera", "alltaxa"

    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab", "SSE.mpa" )

    p = spatial_parameters( p=p)

    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ---------------------
  if (DS=="mpa") {
    if (!exists("project.name", p) ) p$project.name = DS
    if (!exists("data_root", p) ) p$data_root = project.datadirectory( "aegis", p$project.name )
    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation

    p$DATA = 'mpa.db( p=p, DS="stmv_inputs" )'
    p$libs = c( p$libs,  project.library( "netmensuration" ))
    p$taxa =  "maxresolved"
    p$map.regions = c("Canada", "USA") # library "map" coastline polygon designations
    p$map.output.directory = file.path( p$project.outdir.root, "maps")
    p$map.palette = colorRampPalette(c("darkblue","blue3", "green", "yellow", "orange","red3", "darkred"), space = "Lab")(100)
    p$map.depthcontours = c( 200, 400, 600 ) # to plot on maps
    p$map.depthcontours.colours = c( "gray90", "gray85", "gray80", "gray74", "gray72", "gray70" )
    p$varstomodel = c() ### TODO ..
    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE.mpa"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = c( "snowcrab" )
    p = spatial_parameters( p=p)
    if (!exists("variables", p)) p$variables = list()
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"
    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model", stmv_dimensionality="space-year" )
    return(p)
  }


  # ----------------------------------
  if ( DS=="stmv_spatial_model" ) {
    # generic stmv settings space AND time interp (and not just spatial)

    p$libs = RLibrary( c( p$libs, "stmv" ) )
    if (!exists("storage.backend", p)) p$storage.backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )
    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("depth.filter", p)) p$depth.filter = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)
    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.75 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 5 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction  = p$stmv_distance_statsgrid *0.75 # this is a half window km
    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(25, 30, 40) # km ... approx guess of 95% AC range
    if (!exists("stmv_nmin", p)) p$stmv_nmin = 200 # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = 8000 # no real upper bound


    # due to formulae being created on the fly, these are required params
    if (!exists("Y", p$variables)) {
      if (exists("stmv_local_modelformula", p))  {
        if (!is.null(p$stmv_local_modelformula)) {
          if (p$stmv_local_modelformula != "none") {
            oo = all.vars( p$stmv_local_modelformula[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }

      if (exists("stmv_local_modelformula_space", p))  {
        if (!is.null(p$stmv_local_modelformula_space)) {
          if (p$stmv_local_modelformula_space != "none") {
            oo = all.vars( p$stmv_local_modelformula_space[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }

      if (exists("stmv_local_modelformula_time", p))  {
        if (!is.null(p$stmv_local_modelformula_time)) {
          if (p$stmv_local_modelformula_time != "none") {
            oo = all.vars( p$stmv_local_modelformula_time[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }

      if (exists("stmv_global_modelformula", p))  {
        if (!is.null(p$stmv_global_modelformula)) {
          if (p$stmv_global_modelformula != "none") {
            oo = all.vars( p$stmv_global_modelformula[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }
    }
    if (!exists("Y", p$variables)) p$variables$Y = "not_defined" # this can be called to get covars.. do not stop

    p = aegis_modelformula(p=p)  # use generic models if none are specified
    p = stmv_variablelist(p=p)  # decompose into covariates from formulas , etc

    return(p)
  }


  # ----------------------------------
  if ( DS=="stmv_spatiotemporal_model" ) {
    # generic stmv settings space AND time interp (and not just spatial), mostly for aegis "indicators"

    p$libs = RLibrary( c( p$libs, "stmv" ) )

    if (!exists("storage.backend", p)) p$storage.backend="bigmemory.ram"
    if (!exists("clusters", p)) p$clusters = rep("localhost", detectCores() )
    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("depth.filter", p)) p$depth.filter = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)

    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.2 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 2 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )
    # if (!exists("stmv_distance_prediction", p)) p$stmv_distance_prediction = p$stmv_distance_statsgrid *0.75 # this is a half window km
    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(50, 60, 75, 100) # km ... approx guess of 95% AC range .. data tends to be sprse realtive to pure space models

    # lookup temporal params for the SSE domain
    if (!exists("data.sources", p)) p$data.sources = c("groundfish", "snowcrab")
    # obtain current temperature years

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)

    if (!exists("stmv_nmin", p)) p$stmv_nmin = floor( 7 * p$ny ) # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = max( floor( 7 * p$ny ) * 11, 8000) # no real upper bound

    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = {c(1:p$nw)-1} / p$nw # intervals of decimal years... fractional year breaks
    p$dyear_centre = p$dyears[ round(p$nw/2) ] + p$tres/2

    if (!exists("prediction.dyear", p)) p$prediction.dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()

    # must specify, else assumed = 1 (1= no time)
    if (!exists("stmv_dimensionality", p)) p$stmv_dimensionality = "space-year"  # default

    if (p$stmv_dimensionality == "space-year") {
      ## nt=ny annual time steps (default)
      if (!exists("nt", p)) p$nt = p$ny   # ie, default is an annual model (no p$nw)
      # output timeslices for predictions in decimla years, yes all of them here
      if (!exists("prediction.ts", p)) p$prediction.ts = p$yrs + p$prediction.dyear
    } else if (p$stmv_dimensionality == "space-year-season") {
      ## nt = ny*nw is seassonal

      if (!exists("nt", p)) p$nt = p$nw*p$ny # i.e., seasonal with p$nw (default is annual: nt=ny)
      tout = expand.grid( yr=p$yrs, dyear=1:p$nw, KEEP.OUT.ATTRS=FALSE )
      tout$tiyr = tout$yr + tout$dyear/p$nw - p$tres/2 # mid-points
      tout = tout[ order(tout$tiyr), ]
      # output timeslices for predictions in decimla years, yes all of them here
      if (!exists("prediction.ts", p)) p$prediction.ts = tout$tiyr   # predictions at these time values (decimal-year)
    }

    # global model options
    # using covariates as a first pass essentially makes it ~ kriging with external drift
    # .. no space or time here .. only in the local model
    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine ="gam" #default
    if (!exists("stmv_global_family", p)) p$stmv_global_family = gaussian()

    ## local model-specific options
    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine ="twostep"
    if (p$stmv_local_modelengine =="twostep") {
      # if (!exists("stmv_twostep_space", p)) p$stmv_twostep_space = "fft"
      # if (!exists("stmv_twostep_space", p)) p$stmv_twostep_space = "tps"
      # if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "krige"  # warning .. kind of slow

      if (!exists("stmv_twostep_time", p))  p$stmv_twostep_time = "gam"

      if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "fft" #  matern, krige (very slow), lowpass, lowpass_matern
      if (!exists("stmv_fft_filter", p))  p$stmv_fft_filter="matern"  #  matern, krige (very slow), lowpass, lowpass_matern
      if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE


    }  else if (p$stmv_local_modelengine == "gam") {
      if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer= c("outer", "bfgs")
      if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE
    }  else if (p$stmv_local_modelengine == "bayesx") {
      if (!exists("stmv_local_model_bayesxmethod", p)) p$stmv_local_model_bayesxmethod="MCMC"
      if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = FALSE
    }

    # due to formulae being created on the fly, these are required params
    if (!exists("Y", p$variables)) {
      if (exists("stmv_local_modelformula", p))  {
        if (!is.null(p$stmv_local_modelformula)) {
          if (p$stmv_local_modelformula != "none") {
            oo = all.vars( p$stmv_local_modelformula[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }
      if (exists("stmv_local_modelformula_space", p))  {
        if (!is.null(p$stmv_local_modelformula_space)) {
          if (p$stmv_local_modelformula_space != "none") {
            oo = all.vars( p$stmv_local_modelformula_space[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }
      if (exists("stmv_local_modelformula_time", p))  {
        if (!is.null(p$stmv_local_modelformula_time)) {
          if (p$stmv_local_modelformula_time != "none") {
            oo = all.vars( p$stmv_local_modelformula_time[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }

      if (exists("stmv_global_modelformula", p))  {
        if (!is.null(p$stmv_global_modelformula)) {
          if (p$stmv_global_modelformula != "none") {
            oo = all.vars( p$stmv_global_modelformula[[2]] )
            if (length(oo) > 0) p$variables$Y = oo
          }
        }
      }
    }
    if (!exists("Y", p$variables)) p$variables$Y = "not_defined" # this can be called to get covars.. do not stop

    p = aegis_modelformula(p=p)  # use generic models if none are specified
    p = stmv_variablelist(p=p)  # decompose into covariates, etc

    return(p)
  }

}
