
aegis_stmv = function( DS=NULL, p=NULL, year=NULL, ret="mean", varnames=NULL, coastline_source="eastcoast_gadm", alldata=FALSE, redo=FALSE, ... ) {

  # prep and post-process aegis data for stmv and stmv to aegis biologicals
  # based upo snowcrab_stmv
  # deal with additional passed parameters
  # ---------------------

  if ( is.null(p) ) p=list()
  p_add = list(...)
  if (length(p_add) > 0 ) p = c(p, p_add)
  i = which(duplicated(names(p), fromLast = TRUE ) )
  if ( length(i) > 0 ) p = p[-i] # give any passed parameters a higher priority, overwriting pre-existing variable

  # ---------------------
  if (exists( "libs", p)) RLibrary( p$libs )
  if (!("stmv" %in% p$libs)) p$libs = c( p$libs, RLibrary( "stmv" ) )  # required for parallel processing

  if (!exists("variables", p)) p$variables = list()

  if (!exists("LOCS", p$variables)) p$variables$LOCS = c("plon", "plat")
  if (!exists("TIME", p$variables)) p$variables$TIME = "tiyr"


  # ---------------------

  if (DS=="parameters") {

    if (0) {
      # to remove ?
      p$season = "summer"
      p$taxa =  "maxresolved"
      # p$nw = 10  # from temperature.r, number of intervals in a year
      p$clusters = rep("localhost", detectCores() )
      p$varstomodel = c()
      p$taxa.of.interest = aegis.survey::groundfish.variablelist("catch.summary")

      if (!exists("polygon_source", p)) p$polygon_source = "pre2014"   # "pre2014" for older
      if (!exists("internal.crs", p)) p$internal.crs = "+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs"
      if (!exists("proj4string_planar_km", p)) p$proj4string_planar_km = "+proj=omerc +lat_0=44.0 +lonc=-63.0 +gamma=0.0 +k=1 +alpha=325 +x_0=0 +y_0=0 +ellps=WGS84 +units=km"  # oblique mercator, centred on Scotian Shelf rotated by 325 degrees
      if (!exists("boundingbox", p)) p$boundingbox = list( xlim = c(-70.5, -56.5), ylim=c(39.5, 47.5)) # bounding box for plots using spplot
      if (!exists("trawlable_units", p)) p$trawlable_units = "standardtow"  # for groundfish.db


      # set up default map projection
      p = c(p, aegis.coastline::coastline_layout( p=p, redo=redo ))
      p$mypalette=RColorBrewer::brewer.pal(9, "YlOrRd")

      if (!exists("varstomodel", p) ) p$varstomodel = c( "pca1", "pca2", "ca1", "ca2" )
    }


    p$libs = c( p$libs, RLibrary ( "sp", "spdep", "rgeos", "INLA", "mgcv" ))  # standard CRAN libs -- geostatistical support
    p$libs = c( p$libs, RLibrary( "aegis", "bio.taxonomy", "carstm", "stmv", "netmensuration" ) ) #,  ) # locally developed code

    if (!exists("project.name", p) ) stop("project.name is required")
    if (!exists("data_root", p) ) stop( "data_root is required")
    if (!exists("variables", p) ) stop( "variables is required")
    if (!exists("Y", p$variables) ) stop( "variables$Y is required")
    if (!exists("LOCS", p$variables)) p$variables$LOCS=c("plon", "plat")
    if (!exists("TIME", p$variables)) p$variables$TIME="tiyr"

    if (!exists("spatial.domain", p) ) p$spatial.domain = "SSE"
    if (!exists("spatial.domain.subareas", p)) p$spatial.domain.subareas = NULL #c( "snowcrab" )

    p = spatial_parameters( p=p )

    if (!exists("stmv_dimensionality", p))  p$stmv_dimensionality="space-year"

    p = aegis_parameters(p=p, DS="stmv_spatiotemporal_model" )

    if (!exists("storage.backend", p)) p$storage.backend="bigmemory.ram"

    if (!exists("boundary", p)) p$boundary = FALSE
    if (!exists("stmv_filter_depth_m", p)) p$stmv_filter_depth_m = 0 # depth (m) stats locations with elevation > 0 m as being on land (and so ignore)

    if (!exists("stmv_rsquared_threshold", p)) p$stmv_rsquared_threshold = 0.25 # lower threshold
    if (!exists("stmv_distance_statsgrid", p)) p$stmv_distance_statsgrid = 4 # resolution (km) of data aggregation (i.e. generation of the ** statistics ** )

    if (!exists("stmv_distance_scale", p)) p$stmv_distance_scale = c(25, 35, 45) # km ... approx guess of 95% AC range

    if (!exists("stmv_nmin", p)) p$stmv_nmin = 250 # stmv_nmin/stmv_nmax changes with resolution must be more than the number of knots/edf
    # min number of data points req before attempting to model timeseries in a localized space
    if (!exists("stmv_nmax", p)) p$stmv_nmax = 6000 # actually can have a lot of data from logbooks ... this keeps things reasonable in terms of run-time

    if (!exists("habitat.threshold.quantile", p)) p$habitat.threshold.quantile = 0.05


    # additional variable to extract from aegis_db for inputs
    p$aegis_variables = list()
    # p$aegis_project_datasources = c("speciescomposition", "speciesarea", "sizespectrum", "condition", "metabolism", "biochem")
    if (!exists("aegis_project_datasources", p)) p$aegis_project_datasources = "speciescomposition"
    for (id in p$aegis_project_datasources ) {
      pz = aegis_parameters( p=p, DS=id )
      pz_vars = intersect( pz$varstomodel, p$variables$COV )  # these are aegis vars to model
      if (length(pz_vars) > 0) p$aegis_variables[[id]] = pz_vars
    }

    if (!exists("stmv_local_modelengine", p)) p$stmv_local_modelengine ="gam"

    if (!exists("stmv_global_modelengine", p)) p$stmv_global_modelengine ="gam"
    if (!exists("stmv_global_family", p)) p$stmv_global_family = gaussian(link="log")

    # using covariates as a first pass essentially makes it ~ kriging with external drift .. no time or space here
    if (!exists("stmv_global_modelformula", p)) {
      p$stmv_global_modelformula = formula( paste(
        p$variables$Y,
        ' ~ offset(wt) ',
        ' + s( t, k=3, bs="ts") + s( tsd, k=3, bs="ts") + s( tmax, k=3, bs="ts") + s( degreedays, k=3, bs="ts") ',
        ' + s( log(z), k=3, bs="ts") + s( log(dZ), k=3, bs="ts") + s( log(ddZ), k=3, bs="ts") ',
        ' + s( log(substrate.grainsize), k=3, bs="ts") + s(pca1, k=3, bs="ts") + s(pca2, k=3, bs="ts")  '
      ))  # no space
    }

      if (p$stmv_local_modelengine =="twostep") {
        # this is the time component (mostly) .. space enters as a rough constraint
        # if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
        #   p$variables$Y, '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
        #     ' + s(cos.w, sin.w, yr, bs="ts", k=20) ',
        #     ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=20, bs="ts") ' ) )
        if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE
        if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer=c("outer", "bfgs")

        if (!exists("stmv_twostep_time", p))  p$stmv_twostep_time = "gam"

        if (p$stmv_twostep_time == "gam") {
          if (!exists("stmv_local_modelformula_time", p)) {
            p$stmv_local_modelformula_time = formula( paste(
              p$variables$Y,
              ' ~ s(yr, k=12, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
              ' + s(cos.w, sin.w, yr, bs="ts", k=30) ',
              ' + s( log(z), k=3, bs="ts" ) + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(log(z), plon, plat, k=30, bs="ts") '
            ) )
          }
        }

        # this is the spatial component
        if (!exists("stmv_twostep_space", p))  p$stmv_twostep_space = "fft"
        if (p$stmv_twostep_space == "gam") {
          if (!exists("stmv_local_modelformula_space", p))  p$stmv_local_modelformula_space = formula( paste(
          p$variables$Y, '~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
        }
        if (!exists("stmv_fft_filter", p)) p$stmv_fft_filter="matern" #  matern, krige (very slow), lowpass, lowpass_matern

      }  else if (p$stmv_local_modelengine == "gam") {

        if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
          p$variables$Y, '~ s(yr, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
            ' + s(cos.w, sin.w, yr, bs="ts", k=25)  ',
            ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=25, bs="ts") ' ) )
        if (!exists("stmv_local_model_distanceweighted", p)) p$stmv_local_model_distanceweighted = TRUE
        # if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer="perf"
        if (!exists("stmv_gam_optimizer", p)) p$stmv_gam_optimizer=c("outer", "bfgs")

      }  else if (p$stmv_local_modelengine == "bayesx") {
        # bayesx families are specified as characters, this forces it to pass as is and
        # then the next does the transformation internal to the "stmv__bayesx"
        # alternative models .. testing .. problem is that SE of fit is not accessible?
        p$stmv_local_modelformula = formula( paste(
          p$variables$Y, ' ~ sx(yr, bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps") +s(z, bs="ps") + sx(plon, bs="ps") + sx(plat,  bs="ps")',
            ' + sx(plon, plat, cos.w, sin.w, yr, bs="te") ' )
            # te is tensor spline
        )
        p$stmv_local_model_bayesxmethod="MCMC"
        p$stmv_local_model_distanceweighted = FALSE
      } else {
        message( "The specified stmv_local_modelengine is not tested/supported ... you are on your own ;) ..." )
      }

      p = stmv_variablelist(p=p)  # decompose into covariates, etc

    return(p)
  }


  # --------------------------

  if (DS=="stmv_inputs") {

    fn = file.path( project.datadirectory(p$data_root, "aegis_stmv" ), paste( "stmv_inputs", p$selection$type, "rdata", sep="." ) )
    out = NULL

    if (!redo) {
      if (file.exists(fn)) load(fn)
      if( !is.null(out) ) return(out)
    }

    # extract covariates and supplent survey data via lookups
    set = survey.db( p=p, DS="filter" )
    set = set[ which(is.finite(set$lon + set$lat)),]
    # ensure we have some estimate of sweptarea and choose the appropriate
    # one based upon which trawlable units we are using

    if ( p$selection$type=="number")  {
      set$wt = 1 / set$cf_set_no
    }
    if ( p$selection$type=="biomass") {
      set$wt = 1 / set$cf_set_mass
    }
    if ( p$selection$type=="presence_absence")  {
      pa = presence.absence( X=set$zm, px=p$habitat.threshold.quantile )  # determine presence absence and weighting
      set$pa = pa$pa
      set$wt = pa$probs
      pa = NULL
    }
    if ( p$selection$type=="direct")  {
      # for z-scores: zn or zm
      ft2m = 0.3048
      m2km = 1/1000
      nmi2mi = 1.1507794
      mi2ft = 5280
      standardtow_sakm2 = (41 * ft2m * m2km ) * ( 1.75 * nmi2mi * mi2ft * ft2m * m2km )  # surface area sampled by a standard tow in km^2  1.75 nm
      set$wt = switch( p$trawlable_units,
        standardtow =  rep(standardtow_sakm2, nrow(set)) , # "standard tow"
        towdistance = set$sa_towdistance,  # "sa"=computed from tow distance and standard width, 0.011801==),
        sweptarea = set$sa  # swept area based upon stand tow width and variable lenths based upon start-end locations wherever possible
      )
    }
    set$wt[which(!is.finite(set$wt))] = median(set$wt, na.rm=TRUE )  # just in case missing data
    set$data_offset = set$wt

    set = set[which(is.finite(set[,p$variables$Y])) ,]

    if (exists("quantile_bounds", p)) {
      highestpossible = quantile( set[,p$variables$Y], probs=p$quantile_bounds[2], na.rm=TRUE )
      set[,p$variables$Y][ set[,p$variables$Y] > highestpossible ] = highestpossible
      # keep "zero's" to inform spatial processes but only as "lowestpossible" value
      jj = which( set[,p$variables$Y] > 0 )
      lowestpossible =  quantile( set[,p$variables$Y][jj], probs=p$quantile_bounds[1], na.rm=TRUE )
      lowerbound =  quantile( set[,p$variables$Y][jj], probs=p$quantile_bounds[1]/10, na.rm=TRUE )
      ii = which( set[,p$variables$Y] < lowestpossible )
      set[,p$variables$Y][ii] = lowerbound ## arbitrary but close to detection limit
    }

    coast = coastline.db( p=p, DS=coastline_source )
    coast = spTransform( coast, CRS("+proj=longlat +datum=WGS84") )
    setcoord = SpatialPoints( as.matrix( set[, c("lon", "lat")]),  proj4string=CRS("+proj=longlat +datum=WGS84") )
    inside = sp::over( setcoord, coast )
    onland = which (is.finite(inside))
    if (length(onland)>0) set = set[-onland, ]

    set$tiyr = lubridate::decimal_date( set$timestamp )

    set = aegis_db_lookup(
      X=set,
      lookupvars=p$variables$COV,
      xy_vars=c("lon", "lat"),
      time_var="timestamp"
    )

    vars = c( p$variables$LOCS, p$variables$COV, p$variables$Y, p$variables$TIME, "dyear", "yr",  "wt", "data_offset")

    set = set[, which(names(set) %in% vars ) ]  # a data frame
    oo = setdiff( c( p$variables$LOCS, p$variables$COV ), names(set))
    if (length(oo) > 0 ) {
      print(oo )
      warning("Some variables are missing in the input data")
    }
    set = na.omit(set)


    # cap quantiles of dependent vars
    if (exists("quantile_bounds", p)) {
      dr = list()
      for (pvn in p$variables$COV) {
        dr[[pvn]] = quantile( set[,pvn], probs=p$quantile_bounds, na.rm=TRUE ) # use 95%CI
        il = which( set[,pvn] < dr[[pvn]][1] )
        if ( length(il) > 0 ) set[il,pvn] = dr[[pvn]][1]
        iu = which( set[,pvn] > dr[[pvn]][2] )
        if ( length(iu) > 0 ) set[iu,pvn] = dr[[pvn]][2]
      }
    }

    LOCS = bathymetry.db(p=p, DS="baseline")

    # collapse PS vars with time into APS (and regrid via raster)
    PS = aegis_db_extract(
      vars=p$variables$COV,
      yrs=p$yrs,
      spatial.domain=p$spatial.domain,
      dyear=p$prediction.dyear
    )
    PS$data_offset = rep( p$pres*p$pres, nrow(LOCS) )  # force to be density n/km^2
    out = list(input=set, output=list( LOCS=LOCS, COV=PS ))
    save(out, file=fn, compress=TRUE)
    return (out)
  }


  # -----------------------------------


  if ( DS %in% c("predictions", "predictions.redo" ) ) {
    # NOTE: the primary interpolated data were already created by stmv.
    # This routine points to this data and also creates
    # subsets of the data where required, determined by "spatial.domain.subareas"
    # not strictly required for snow crab data analysis as there are no sub-areas that are analysed
    # at present, but in case there are small-area analysis in future, this is a mechnanism

    projectdir = file.path(p$data_root, "modelled", p$variables$Y, p$spatial.domain )

    if (DS %in% c("predictions")) {
      P = Pl = Pu = NULL
      fn = file.path( projectdir, paste("stmv.prediction", ret,  year, "rdata", sep=".") )
      if (file.exists(fn) ) load(fn)
      if (ret=="mean") return (P)
      if (ret=="lb") return( Pl)
      if (ret=="ub") return( Pu)
    }

    sreg = setdiff( p$spatial.domain.subareas, p$spatial.domain ) # see  note above
    if (is.null(sreg)) return
    if (length(sreg) < 1 ) return

    p0 = spatial_parameters( p=p ) # make explicit
    L0 = bathymetry.db( p=p0, DS="baseline" )
    L0i = stmv::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )

    for ( year in p$yrs ) {
      # print (year)
      # default domain
      PP0 = stmv_db( p=p, DS="stmv.prediction", yr=year, ret="mean")
      VV0 = stmv_db( p=p, DS="stmv.prediction", yr=year, ret="lb")
      WW0 = stmv_db( p=p, DS="stmv.prediction", yr=year, ret="ub")

      for ( gr in sreg ) {
        # warping
        p1 = spatial_parameters( p=p, spatial.domain=gr ) # 'warping' from p -> p1
        L1 = bathymetry.db( p=p1, DS="baseline" )
        L1i = stmv::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
        L1 = planar2lonlat( L1, proj.type=p1$internal.crs )
        L1$plon_1 = L1$plon # store original coords
        L1$plat_1 = L1$plat
        L1 = lonlat2planar( L1, proj.type=p0$internal.crs )
        p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
          # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy

        P = spatial_warp( PP0[], L0, L1, p0, p1, "fast", L0i, L1i )
        Pl = spatial_warp( VV0[], L0, L1, p0, p1, "fast", L0i, L1i )
        Pu = spatial_warp( WW0[], L0, L1, p0, p1, "fast", L0i, L1i )
        projectdir_p1 = file.path(p$data_root, "modelled", p1$variables$Y, p1$spatial.domain )
        dir.create( projectdir_p1, recursive=T, showWarnings=F )
        fn1_sg = file.path( projectdir_p1, paste("stmv.prediction.mean",  year, "rdata", sep=".") )
        fn2_sg = file.path( projectdir_p1, paste("stmv.prediction.lb",  year, "rdata", sep=".") )
        fn3_sg = file.path( projectdir_p1, paste("stmv.prediction.ub",  year, "rdata", sep=".") )
        save( P, file=fn1_sg, compress=T )
        save( Pl, file=fn2_sg, compress=T )
        save( Pu, file=fn3_sg, compress=T )
        print (fn1_sg)
      }
    }

    return ("Completed")

    if (0) {
      levelplot( P ~ plon_1 + plat_1, L1, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    }

  }


  #  -------------------------------

  if (DS %in% c(  "stmv.stats", "stmv.stats.redo" )){

    if (DS %in% c("stmv.stats")) {
      stats = NULL
      projectdir = file.path(p$data_root, "modelled", p$variables$Y, p$spatial.domain )
      fn = file.path( projectdir, paste( "stmv.statistics", "rdata", sep=".") )
      if (file.exists(fn) ) load(fn)
      return( stats )
    }

    sreg = setdiff( p$spatial.domain.subareas, p$spatial.domain )
    if (is.null(sreg)) return
    if (length(sreg) < 1 ) return

    S0 = stmv_db( p=p, DS="stmv.stats" )
    Snames = colnames(S0)
    p0 = spatial_parameters( p=p ) # from
    L0 = bathymetry.db( p=p0, DS="baseline" )
    L0i = stmv::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )

    for ( gr in sreg ) {
      p1 = spatial_parameters( p=p, spatial.domain=gr ) # 'warping' from p -> p1
      L1 = bathymetry.db( p=p1, DS="baseline" )
      L1i = stmv::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
      L1 = planar2lonlat( L1, proj.type=p1$internal.crs )
      L1$plon_1 = L1$plon # store original coords
      L1$plat_1 = L1$plat
      L1 = lonlat2planar( L1, proj.type=p0$internal.crs )
      p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
        # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy
      stats = matrix( NA, ncol=ncol(S0), nrow=nrow(L1) )
      for ( i in 1:ncol(S0) ) {
        stats[,i] = spatial_warp( S0[,i], L0, L1, p0, p1, "fast", L0i, L1i )
      }
      colnames(stats) = Snames
      projectdir_p1 = file.path(p$data_root, "modelled", p$variables$Y, p1$spatial.domain )
      dir.create( projectdir_p1, recursive=T, showWarnings=F )
      fn1_sg = file.path( projectdir_p1, paste("stmv.statistics", "rdata", sep=".") )
      save( stats, file=fn1_sg, compress=T )
      print (fn1_sg)
    }
    return ("Completed")

    if (0) {
      levelplot( stats[,1] ~ plon_1 + plat_1, L1, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
    }
  }

  #  -------------------------------

  if (DS %in% c("complete", "complete.redo") ) {
    # assemble data for use by other projects
    if (DS=="complete") {
      IC = NULL
      projectdir = file.path(p$data_root, "modelled", p$variables$Y, p$spatial.domain )
      dir.create(projectdir, recursive=T, showWarnings=F)
      outfile =  file.path( projectdir, paste( p$project.name, "complete", p$spatial.domain, "rdata", sep= ".") )
      if ( file.exists( outfile ) ) load( outfile )
      Inames = names(IC)
      if (is.null(varnames)) varnames=Inames
      varnames = intersect( Inames, varnames )
      if (length(varnames) == 0) varnames=Inames  # no match .. send all
      IC = IC[ , varnames]
      return(IC)
    }

    grids = unique( c(p$spatial.domain.subareas , p$spatial.domain ) ) # operate upon every domain

    for (gr in grids ) {
      p1 = spatial_parameters( p=p, spatial.domain=gr ) #target projection
      # p1$variables$Y = p$variables$Y # need to send this to get the correct results
      L1 = bathymetry.db(p=p1, DS="baseline")
      BS = aegis_stmv( p=p1, DS="stmv.stats" )
      colnames(BS) = paste(p$variables$Y, colnames(BS), sep=".")
      IC = cbind( L1, BS )
      # climatology
      nL1 = nrow(L1)
      PS = PSlb = PSub = matrix( NA, nrow=nL1, ncol=p$ny )
      for (iy in 1:p$ny) {
        yr = p$yrs[iy]
        PS[,iy] = stmv_db( p=p1, DS="stmv.prediction", yr=yr, ret="mean")
        PSlb[,iy] = stmv_db( p=p1, DS="stmv.prediction", yr=yr, ret="lb")
        PSub[,iy] = stmv_db( p=p1, DS="stmv.prediction", yr=yr, ret="ub")
      }
      CL = cbind( apply( PS, 1, mean, na.rm=TRUE ),
                  apply( PSlb, 1, mean, na.rm=TRUE ),
                  apply( PSub, 1, mean, na.rm=TRUE ) )
      colnames(CL) = paste( p1$variables$Y, c("mean", "lb", "ub"), "climatology", sep=".")
      IC = cbind( IC, CL )
      PS = PSlb = PSub = NULL
      projectdir = file.path(p$data_root, "modelled", p1$variables$Y, p1$spatial.domain )
      dir.create( projectdir, recursive=T, showWarnings=F )
      outfile =  file.path( projectdir, paste( p$project.name, "complete", p1$spatial.domain, "rdata", sep= ".") )
      save( IC, file=outfile, compress=T )
      print( outfile )
    }
    return( "Complete" )
  }

  # -------------------

  if (DS %in% c("baseline", "baseline.redo") ) {

    if ( DS=="baseline" ) {
      BL = list()
      for (bvn in varnames ) {
        projectdir = file.path(p$data_root, "modelled", bvn, p$spatial.domain )
        outfile =  file.path( projectdir, paste( p$project.name, "baseline", ret, p$spatial.domain, "rdata", sep= ".") )
        TS = NULL
        load( outfile)
        BL[[bvn]] = TS
      }
      return (BL)
    }

    grids = unique( c(p$spatial.domain.subareas , p$spatial.domain ) ) # operate upon every domain

    for (gr in grids) {
        print(gr)
        p1 = spatial_parameters( p=p, spatial.domain=gr ) #target projection
        projectdir = file.path(p$data_root, "modelled", p$variables$Y, p1$spatial.domain )
        dir.create( projectdir, recursive=T, showWarnings=F )
        L1 = bathymetry.db(p=p1, DS="baseline")
        nL1 = nrow(L1)
        TS = matrix( NA, nrow=nL1, ncol=p$ny )
        for (i in 1:p$ny ) {
          TS[,i] = stmv_db( p=p1, DS="stmv.prediction", yr=p$yrs[i], ret="mean")
        }
        outfile =  file.path( projectdir, paste( p$project.name, "baseline", "mean", p1$spatial.domain, "rdata", sep= ".") )
        save( TS, file=outfile, compress=T )
        TS = TS[] * NA
        for (i in 1:p$ny ) {
          TS[,i] = stmv_db( p=p1, DS="stmv.prediction", yr=p$yrs[i], ret="lb")
        }
        outfile =  file.path( projectdir, paste( p$project.name, "baseline", "lb", p1$spatial.domain, "rdata", sep= ".") )
        save( TS, file=outfile, compress=T )
        TS = TS[] * NA
        for (i in 1:p$ny ) {
          TS[,i] = stmv_db( p=p1, DS="stmv.prediction", yr=p$yrs[i], ret="ub")
        }
        outfile =  file.path( projectdir, paste( p$project.name, "baseline", "ub", p1$spatial.domain, "rdata", sep= ".") )
        save( TS, file=outfile, compress=T )
      }
    return( "Complete" )
  }


  # -----------------------

  if ( DS=="map.all" ) {

    allgrids = unique(c( p$spatial.domain.subareas, p$spatial.domain) )
    for ( gr in allgrids ) {
      print (gr)
      p1 = spatial_parameters(  p=p, spatial.domain= gr )
      aegis_stmv( p=p1, DS="map.climatology" ) # no parallel option .. just a few
      aegis_stmv( p=p1, DS="map.annual" )
    }

  }

  # -----------------------


  if ( DS %in% c("map.annual" ) ) {

    annot.cex=0.65
    eps = 0.001

    for ( year in p$yrs ) {
        projectdir = file.path(p$data_root, "maps", p$variables$Y, p$spatial.domain, "annual" )
        dir.create( projectdir, recursive=T, showWarnings=F )
        loc = bathymetry.db(p=p, DS="baseline" )

        # downscale and warp from p(0) -> p1
        # print(year)
        H = aegis_stmv( p=p, DS="predictions", year=year, ret="mean" )
        if (is.null(H)) next ()
        # H = log(H)
        xyz = cbind(loc, H)
        uu = which( is.finite(rowSums(xyz)))
        if (length(uu) < 10) next()
        xyz = xyz[uu,]
        datarange = NULL
        # datarange = snowcrab.lookup.mapparams( DS="datarange", p$variables$Y ) # hardcoded data ranges
        if (is.null(datarange)) {
          datarange=quantile(xyz[,3], probs=c(0.001,0.999), na.rm=TRUE)
          datarange = seq( datarange[1], datarange[2], length.out=100 )
        }
        cols = color.code( "blue.black", datarange )
        annot = gsub( ".", " ", toupper(p$variables$Y), fixed=TRUE )
        outfn = paste( p$variables$Y, "mean", year, sep=".")

        dir.create (projectdir, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( projectdir, paste(outfn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial.domain=p$spatial.domain , plotlines="cfa.regions"  )
        print(lp)
        dev.off()

        H = aegis_stmv( p=p, DS="predictions", year=year, ret="lb" )
        if (is.null(H)) next ()
        # H = log(H)
        xyz = cbind(loc, H)
        uu = which( is.finite(rowSums(xyz)))
        if (length(uu) < 10) next()
        xyz = xyz[uu,]
        datarange = NULL
        # datarange = snowcrab.lookup.mapparams( DS="datarange", p$variables$Y ) # hardcoded data ranges
        if (is.null(datarange)) {
          datarange=quantile(xyz[,3], probs=c(0.001,0.999), na.rm=TRUE)
          if (diff(datarange) < eps) datarange[2] = datarange[2]+ eps
          datarange = seq( datarange[1], datarange[2], length.out=100 )
        }
        cols = color.code( "blue.black", datarange )
        annot = gsub( ".", " ", toupper(p$variables$Y), fixed=TRUE )
        outfn = paste( p$variables$Y, "lb", year, sep=".")

        dir.create (projectdir, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( projectdir, paste(outfn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial.domain=p$spatial.domain , plotlines="cfa.regions" )
        print(lp)
        dev.off()

        H = aegis_stmv( p=p, DS="predictions", year=year, ret="ub" )
        if (is.null(H)) next ()
        # H = log(H)
        xyz = cbind(loc, H)
        uu = which( is.finite(rowSums(xyz)))
        if (length(uu) < 10) next()
        xyz = xyz[uu,]
        datarange = NULL
        # datarange = snowcrab.lookup.mapparams( DS="datarange", p$variables$Y ) # hardcoded data ranges
        if (is.null(datarange)) {
          datarange=quantile(xyz[,3], probs=c(0.001,0.999), na.rm=TRUE)
          datarange = seq( datarange[1], datarange[2], length.out=100 )
        }
        cols = color.code( "blue.black", datarange )
        annot = gsub( ".", " ", toupper(p$variables$Y), fixed=TRUE )
        outfn = paste( p$variables$Y, "ub", year, sep=".")

        dir.create (projectdir, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( projectdir, paste(outfn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
              annot=annot, annot.cex=annot.cex, at=datarange , col.regions=cols,
              corners=p$corners, spatial.domain=p$spatial.domain , plotlines="cfa.regions" )
        print(lp)
        dev.off()

        print( file.path( projectdir, outfn))
    }
    return("Finished")
  }


  # ------------------------------


  if ( DS %in% c("map.climatology" ) ) {

    annot.cex=0.75
    eps = 0.001

    H = aegis_stmv( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))
    H = NULL

    for (vn in vnames) {
        projectdir = file.path(p$data_root, "maps", p$variables$Y, p$spatial.domain, "climatology" )
        dir.create( projectdir, recursive=T, showWarnings=F )
        loc = bathymetry.db(p=p, DS="baseline" )
        H = aegis_stmv( p=p, DS="complete" )
        vnames = setdiff( names(H), c("plon", "plat" ))

        xyz = cbind(loc, H[,vn])
        # if (grepl("abundance", vn)) xyz[,3] = log(xyz[,3])
        uu = which( is.finite(rowSums(xyz)))
        if (length(uu) < 10) next()
        xyz = xyz[uu,]
        datarange= NULL
        # datarange = snowcrab.lookup.mapparams( DS="datarange", vn) # hardcoded data ranges
        if (is.null(datarange)) {
          datarange=quantile(xyz[,3], probs=c(0.005,0.995), na.rm=TRUE)
          if (diff(datarange) < eps) datarange[2] = datarange[2]+ eps
          datarange = seq( datarange[1], datarange[2], length.out=100 )
        }
        cols = color.code( "blue.black", datarange )
        annot = gsub( ".", " ", toupper(vn), fixed=TRUE )

        dir.create (projectdir, showWarnings=FALSE, recursive =TRUE)
        fn = file.path( projectdir, paste(vn, "png", sep="." ) )
        png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial.domain=p$spatial.domain, plotlines="cfa.regions"  )
        print(lp)
        dev.off()

        print( fn )
    }
    return( "Completed")
  }
}
