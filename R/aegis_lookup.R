
aegis_lookup = function( p, DS="all", locsmap=NULL, locs=NULL, timestamp=NULL, varnames=NULL, DB=NULL, ret="mean" ) {

  # lookup from "stmv" results

  if (0) {
    # example of how to use this:
    set = survey_db( p=p, DS="set" )
    newvars = c("tmean.climatology", "tmax.climatology")
    locsmap = match(
      array_map( "xy->1", set[,c("plon","plat")], gridparams=p$gridparams ),
      array_map( "xy->1", bathymetry_db(p=p, DS="baseline"), gridparams=p$gridparams ) )

    # for spatial-only
    sn = aegis_lookup( p=p, DS="spatial", locsmap=locsmap, varnames=newvars )
    names( sn ) = newvars
    set = cbind( set,  sn )

    # for space-time(year)
    sn = aegis_lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=set[,"timestamp"], varnames=newvars )
    names( sn  ) = newvars
    set = cbind( set,  sn )
  }

  out = NULL

  if (is.null(locsmap)){
    grid = array_map( "xy->1", locs, gridparams=p$gridparams )
    baid = array_map( "xy->1", bathymetry_db(p=p, DS="baseline"), gridparams=p$gridparams )
    locsmap = match( grid, baid )
  }

  if (DS=="spatial"){
    if (is.null(DB)) DB = aegis_db(p=p, DS="spatial")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    if ( length(varnames) == 1) {
      out[[varnames]] = DB[locsmap,varnames]
    } else if (length(varnames) > 1) {
      out = DB[locsmap,varnames]
    }
    return(out)
  }

  if (DS=="spatial.annual"){
    if (is.null(DB)) DB = aegis_db(p=p, DS="spatial.annual")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    for (vn in varnames){
      DB_years = as.numeric( dimnames(DB[[vn]])[[2]] )
      dindex = cbind(locsmap, match( lubridate::year(timestamp), DB_years ) )
      out = cbind( out, DB[[vn]][dindex] )
    }
    out = data.frame(out)
    names(out) =  varnames
    return(out)
  }

  if (DS=="spatial.annual.seasonal"){
    # only temp for now
    yrs = lubridate::year(timestamp)
    dyear = lubridate::decimal_date( timestamp ) - yrs
    dyear_breaks = c(p$dyears, p$dyears[length(p$dyears)]+ diff(p$dyears)[1] )
    dyear_index = as.numeric( cut( dyear, breaks=dyear_breaks, include.lowest=TRUE, ordered_result=TRUE, right=FALSE ) )
    DB_years = as.numeric( dimnames(DB)[[2]] )
    dindex = cbind(locsmap, match( yrs, DB_years ), dyear_index )

    outnames = NULL
    for (vn in varnames) {
      if (vn=="t") ret = "mean"
      if (vn=="tlb") ret = "lb"
      if (vn=="tub") ret = "ub"
      if (vn %in% c("t", "tlb", "tub"))  p$stmv_variables$Y = "t"  # required to force use of directory "t"
      DB = NULL
      DB=aegis_db(p=p, DS="spatial.annual.seasonal", ret=ret) # at this point this is the only database with seasonality .. other stats (than mean) will require supplemntary functionss
      if (!is.null(DB)) {
        out = cbind( out, DB[dindex] )
        outnames = c(outnames, vn)
      }
    }
    colnames(out) = outnames
    return(out)
  }


  if (DS=="baseline"){
    # all interpolated fields
    DB = aegis_db(p=p, DS="baseline")
    if (is.null(varnames)) varnames=names(DB)
    vnames_DB = names(DB)
    varnames = intersect( vnames_DB, varnames )
    for (vn in varnames){
      DB_years = as.numeric( dimnames(DB[[vn]])[[2]] )
      dindex = cbind(locsmap, match( lubridate::year(timestamp), DB_years ) )
      out = cbind( out, DB[[vn]][dindex] )
    }
    return(out)
  }


  if (DS=="all") {
    # attempt all matches
    out = matrix( NA, ncol=length(varnames), nrow=length(locsmap), dimnames=list(NULL, varnames) )

    sn = NULL
    sn = aegis_lookup( p=p, DS="spatial", locsmap=locsmap, varnames=varnames )
    if (!is.null(sn)) {
      for (vn in names(sn) ) {
        if (is.list(sn)) {
          out[,vn] = sn[[vn]]
        } else {
          out[,vn] = sn[,vn]
        }
      }
    }
    sn = NULL

    # for space-time(year-averages)
    sn = aegis_lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=timestamp, varnames=varnames )
    if (!is.null(sn)) {
      for (vn in names(sn) )  {
        if (is.list(sn)) {
          out[,vn] = sn[[vn]]
        } else {
          out[,vn] = sn[,vn]
        }
      }
    }
    sn = NULL

    # for space-time (year-seasonal)
    vn_sts = intersect( varnames, c("t", "tub", "tlb") )
    if (length(vn_sts)>0) {
      sn = aegis_lookup( p=p, DS="spatial.annual.seasonal", locsmap=locsmap, timestamp=timestamp, varnames=vn_sts )
      if (!is.null(sn)) {
        for (vn in names(sn) )  {
          if (is.list(sn)) {
            out[,vn] = sn[[vn]]
          } else {
            out[,vn] = sn[,vn]
          }
        }
      }
    }

    # additional aegis_db variables
    aegis_project_datasources = c("speciescomposition", "speciesarea", "metabolism", "condition", "sizespectrum")  # check all
    for (apds in aegis_project_datasources ) {
      pz = NULL
      pz = try( aegis_parameters( DS=apds ) )
      if ( is.null(pz) ) next()
      if ( "try-error" %in% class(pz) ) next()
      pz_vars = intersect( pz$varstomodel, varnames )  # these are aegis vars to model
      if (length(pz_vars) > 0) {
        # pa = spatial_parameters( p=pz ) # return to correct domain
        PS = NULL
        PS = aegis_db( p=pz, DS="baseline", varnames=pz_vars )
        if (!is.null(PS)) {
          sn = aegis_lookup( p=pz, DS="spatial.annual", locsmap=locsmap, timestamp=timestamp,
              varnames=pz_vars, DB=PS )
          if (!is.null(sn)) {
            for (vn in names(sn) )  {
              if (is.list(sn)) {
                out[,vn] = sn[[vn]]
              } else {
                out[,vn] = sn[,vn]
              }
            }
          }
        }
      }
    }
    return(out)
  }
}
