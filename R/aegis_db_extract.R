
aegis_db_extract = function( vars, spatial_domain, yrs=NULL, dyear=NULL, dyear_index=NULL, filldata="means", returntype="list",
  areal_units_resolution_km=NULL, aegis_proj4string_planar_km=NULL, redo=FALSE ){

  # based on aegis_db("stmv_inputs") but more generic and dynamic (nothing saved to storage) though slower
  # basically reformatting to match vn and yrs

  # vars=covars; yrs=p$yrs; spatial_domain=p$spatial_domain; dyear=p$prediction_dyear; returntype="data.frame"

  if (0) {
    vars=c("t", "z")
    yrs=1990:2010
    spatial_domain ="SSE"
    dyear=0.6
    # currently supported vars:
    # z = depth (m)
    # dZ = bottom slope (m/km)
    # ddZ = bottom curvature (m/km^2)
    # substrate.grainsize = mean grain size of bottom substrate (mm)
    # t = temperature (C) – subannual
    # tlb = temperature lower 95% bound (C) –subannual
    # tub = temperature upper 95% bound (C) –subannual
    # tmean = mean annual temperature
    # tsd = standard deviation of the mean annual temperature
    # tmin = minimum value of temperature in a given year – annual
    # tmax= maximum value of temperature in a given year – annual
    # tamplitude = amplitude of temperature swings in a year (tmax-tmin) – annual
    # degreedays = number of degree days in a given year – annual

  }

  outputdirectory = getwd()
  fn = file.path( outputdirectory, paste( "aegis_db_extract", spatial_domain, areal_units_resolution_km, returntype, "rdata", sep="." ) )
  message ( "\n Temporary file being made in the current work directory:  ", fn, "\n")

  out = NULL

  if (!redo) {
    if (file.exists(fn)) load(fn)
    if( !is.null(out) ) return(out)
  }


  environmentalvars_static = NULL
  environmentalvars_dynamic_seasonal = NULL
  environmentalvars_dynamic_annual = NULL

  out = list()

  # static vars: depth is the primary constraint, baseline = area-prefiltered for depth/bounds
  pb = aegis.bathymetry::bathymetry_parameters(spatial_domain=spatial_domain )
  PS = aegis_db(p=pb, DS="spatial") # all static variables

  environmentalvars_static = intersect( vars, names(PS) )
  if ( length(environmentalvars_static) > 0 ) {
    out[environmentalvars_static] = PS[environmentalvars_static]
  }
  pb = NULL
  PS = NULL
  gc()

  # dynamic vars .. temp is the basis of all
  if (!is.null(yrs)) {
    pa = aegis.temperature::temperature_parameters( yrs=yrs, spatial_domain=spatial_domain )

    environmentalvars_dynamic_seasonal = intersect( vars, c("t", "tub", "tlb" ) ) ## only ones right now
    if (length(environmentalvars_dynamic_seasonal) > 0) {
      if (is.null(dyear_index)) {
        # priority to dyear_index if provided
        if (is.null(dyear)) {
          if (exists("prediction_dyear", pa)) {
            dyear = pa$prediction_dyear
          }
        }
        if (is.null(dyear)) dyear = 0.7 # catch all
        if (exists("dyears", pa)) {
          dyears = pa$dyears
        } else {
          nw = 10
          dyears = {c(1:nw)-1} / nw
        }
        dyear_index = which.min( abs( dyear - dyears))
      }
      if (is.null(dyear_index)) dyear_index = 7 # if a problem default to this = dyear=0.6 = July

      pa$stmv_variables = list( Y="t" ) # force voi in temperature.db for these
      for (vn in environmentalvars_dynamic_seasonal) {
        # get timeslice
        nlocs = nrow( bathymetry.db(p=pa, DS="baseline"))
        nyears = length(yrs)
        out[[vn]] = matrix(NA, ncol=nyears, nrow=nlocs, dimnames=list(NULL, yrs))
        pa$stmv_variables = list( Y="t" )  # force voi in temperature.db
        if (vn=="t") ret = "mean"
        if (vn=="tlb") ret = "lb"
        if (vn=="tub") ret = "ub"
        for ( r in 1:length(yrs) ) {
          PS = temperature.db( p=pa, DS="predictions", yr=yrs[r], ret=ret  )
          if (!is.null(PS))  out[[vn]][,r] = PS[,dyear_index]
          PS = NULL
        }
      }
    }

    environmentalvars_dynamic_annual = intersect( vars, pa$bstats )
    if ( length(environmentalvars_dynamic_annual) > 0 ) {
      # make years coherent
      pa$stmv_variables = list( Y="t" ) # force voi in temperature.db for bstats
      for (vn in environmentalvars_dynamic_annual) {
        PS = temperature.db( p=pa, DS="bottom.statistics.annual", ret=vn )
        if (is.null(PS)) next()
        nrPS = nrow(PS)
        out[[vn]] = matrix( NA, nrow=nrPS, ncol=length(yrs), dimnames=list(NULL, yrs))
        yr_index = match( yrs, colnames(PS) )
        yg = which(is.finite(yr_index))
        ym = which(is.na(yr_index))
        if (length(yg) > 0) out[[vn]][,yg] = PS[,yr_index[yg]]
        if (length(ym) > 0) {
          if (filldata == "means") out[[vn]][,ym] = rowMeans( out[[vn]][], na.rm=TRUE )  # mean of the selected time period
        }
        PS=NULL
      }
    }
  }

  othervars = setdiff( vars, c(environmentalvars_dynamic_seasonal, environmentalvars_dynamic_annual, environmentalvars_static) )  # dynamic but higher order indicators
  if (length(othervars > 0 )) {

    # aegis_db variables
    aegis_project_datasources = c("speciescomposition", "speciesarea", "metabolism", "condition", "sizespectrum")  # check all
    for (id in aegis_project_datasources ) {
      pz = NULL
      pz = try( aegis_parameters( DS=id, yrs=yrs, spatial_domain=spatial_domain ) )
      if ( is.null(pz) ) next()
      if ( "try-error" %in% class(pz) ) next()
      pz_vars = intersect( pz$varstomodel, othervars )  # these are aegis vars to model
      if (length(pz_vars) > 0) {
        # pa = spatial_parameters( p=pz ) # return to correct domain
        for (vn in pz_vars) {
          PS = aegis_db( p=pz, DS="baseline", varnames=vn )
          out[[vn]] = matrix( NA, nrow=nrow(PS[[vn]]), ncol=length(yrs), dimnames=list(NULL, yrs))
          yr_index = match( yrs, colnames(PS) )
          yg = which(is.finite(yr_index))
          ym = which(is.na(yr_index))
          if (length(yg) > 0) out[[vn]][,yg] = PS[[vn]][,yr_index[yg]]
          if (length(ym) > 0) {
            if (filldata == "means") out[[vn]][, ym] = rowMeans( PS[[vn]], na.rm=TRUE )
          }
          PS =NULL
        }
      }
    }
  }

  if (returntype=="list") {
    save(out, file=fn, compress=TRUE)
    return(out)
  }

  if (returntype=="data.frame" ) {
    # static vars: depth is the primary constraint, baseline = area-prefiltered for depth/bounds
    AS = bathymetry.db( p=aegis.bathymetry::bathymetry_parameters( spatial_domain=spatial_domain ), DS="baseline", varnames=c("plon", "plat") )

    APS = NULL
    ncovars = length(out)
    u = rep(NA, ncovars)
    for ( i in 1:ncovars ) {
      if (is.vector(out[[i]]) ) u[i] = 1
      if (is.matrix(out[[i]]) ) u[i] = ncol(out[[i]])
    }

    j = which( u==1)
    if ( length(j) > 0 ) AS = cbind(AS, as.data.frame(out[j]) )

    k = which( u>1)
    if ( length(k) > 0 ) {
      psnames = names(out[k])
      for (y in 1:length(yrs)) {
        AT = AS
        AT$year = yrs[y]
        oo = NULL
        for( v in k) oo = cbind( oo, out[[v]][,y] )
        oo = as.data.frame(oo)
        names(oo) = psnames
        APS = rbind( APS, cbind(AT, oo) )
      }
    }

    if (!is.null(areal_units_resolution_km) ) {

      spdf0 = SpatialPointsDataFrame( APS[, c("plon", "plat")], data=APS, proj4string=aegis_proj4string_planar_km )
      raster_template = raster(extent(spdf0)) # +1 to increase the area
      res(raster_template) = areal_units_resolution_km  # in units of crs
      crs(raster_template) = projection(spdf0) # transfer the coordinate system to the raster
      out = NULL
      vns = setdiff( names(spdf0), "year" )
      for (y in yrs ) {
        dta = spdf0[ spdf0$year==y ,]
        O = as( rasterize(dta, raster_template, vns, fun=mean, na.rm=TRUE), "SpatialPointsDataFrame" )
        O$year = y
        out = rbind(out, as.data.frame(O) )
      }
    } else {
       out = APS
    }
    save(out, file=fn, compress=TRUE)
    return(out)
  }

  return( "Incorrect choice of output format?")
}
