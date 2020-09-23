
  aegis_db = function( DS="baseline", p=NULL, year=NULL, varnames=NULL, ret=NULL ) {

    # over-ride default dependent variable name if it exists

    # -----------------------------

    if (DS =="indicators") {
      # generic methods getting raw data
      out = switch( p$project_name,
        bathymetry   = bathymetry_db(p=p, DS=p$project_name ),
        temperature  = temperature_db(p=p, DS=p$project_name ),
        sizespectrum = sizespectrum_db(p=p, DS=p$project_name ),
        metabolism   = metabolism_db( p=p, DS=p$project_name ),
        speciesarea  = speciesarea_db( p=p, DS=p$project_name ),
        speciescomposition = speciescomposition_db( p=p, DS=p$project_name ),
        condition =    condition_db( p=p, DS=p$project_name ),
        biochem =      biochem.db( p=p, DS=p$project_name ),
        survey =       survey_db( p=p, DS=p$project_name ),
        NULL
      )
      return(out)
    }


    # -----------------------------


    if (DS %in% c("spatial") ) {
      # spatial only == static variables

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = bathymetry_db( p=p, DS="baseline", varnames="all" )  # anything not NULL gives everything with bathymetry_db
      PS = cbind( PS, substrate_db ( p=p, DS="complete"  ) )

      # override variable of interest to obtain results for static temperature vars
      pt = aegis.temperature::temperature_parameters( p=p)
      pt$stmv_variables = list(Y = "t")
      # p0 = spatial_parameters( p=p0, spatial_domain=p$spatial_domain ) # return to correct domain

      tclim = temperature_db( p=pt, DS="complete" )
      # names(PS)[which(names(PS)=="tamplitude.climatology")] = "tamplitude.climatology"
      PS = cbind( PS, tclim)

      return( PS )
    }


    # -----------------------------


    if (DS %in% c("spatial.annual") ) {
      # spatial and temporal (annual) .. only temperature at this point

      dyear_index = 1
      if (exists("dyears", p) & exists("prediction_dyear", p))  dyear_index = which.min( abs( p$prediction_dyear - p$dyears))

      pt = aegis.temperature::temperature_parameters( p=p )
      pt$stmv_variables = list( Y="t" )
      # pt = spatial_parameters( p=pt, spatial_domain=p$spatial_domain ) # return to correct domain

      PS = list()
      PS[["tslice"]] = temperature_db( p=pt, DS="timeslice", ret="mean", dyear_index=dyear_index ) # at the prediction timeslice
      for ( ret in pt$bstats ) { # ] "tmean"     "tsd"       "tmin"      "tmax"      "tamplitude" "degreedays"
        PS[[ret]] = temperature_db( p=pt, DS="bottom.statistics.annual", ret=ret )
      }

      return( PS )
    }


    #---------------------------------


    if (DS %in% c("spatial.annual.seasonal", "spatial.annual.seasonal.redo") ) {
      #\\ spatial, temporal (annual and seasonal)
      # at present only temperatute varies at this scale
      #\\ copy in array format for domain/resolution of interest for faster lookups

      if ( DS=="spatial.annual.seasonal.redo" ){
        message( "At present only temperature varies at this scale .. nothing else needs to be done." )
        return(NULL)
      }
      out = temperature_db(p=p, DS="spatial.annual.seasonal", ret=ret )
      return( out )
    }


    #---------------------------------


    if (DS %in% c("prediction.surface") ) {

      # depth is the primary constraint, baseline = area-prefiltered for depth/bounds
      PS = aegis_db(p=p, DS="spatial")
      nPS = nrow( PS )
      PS = as.list(PS)

      u = aegis_db(p=p, DS="spatial.annual")
      yr_index = match( as.character(p$yrs), colnames(u[[1]]) )

      # for prediction, a time slice at a particular time of year is elevated as the "t" the temperature at which prediciton will be made
      newnames = names(u)
      newnames[which(newnames=="tslice")] = "t"
      names(u) = newnames

      for ( vn in names(u) )  u[[vn]] = u[[vn]][,yr_index]

      PS = c( PS, u)
      return( PS )
    }


    #  -------------------------------


    if (DS %in% c("stmv_inputs") ) {

      INP = aegis_db( DS="indicators", p=p ) # dependent vars
      INP = INP[ which(INP$yr %in% p$yrs), ]

      INP$tiyr = lubridate::decimal_date( INP$timestamp )

      locsmap = match(
        stmv::array_map( "xy->1", INP[,c("plon","plat")], gridparams=p$gridparams ),
        stmv::array_map( "xy->1", bathymetry_db(p=p, DS="baseline"), gridparams=p$gridparams ) )

      # spatial vars and climatologies
      # sp_vars = intersect( c("dZ", "ddZ", "substrate.grainsize", "tmean.climatology", "tsd.climatology", "b.sd", "b.range", "s.sd", "s.range", "t.range" ), p$stmv_variables$COV )
      newvars = setdiff(p$stmv_variables$COV, names(INP) )
      if (length(newvars) > 0) {
        sn = aegis_lookup( p=p, DS="spatial", locsmap=locsmap, varnames=newvars )
        if (!is.null(sn)) {
          if (ncol(sn) > 0)  INP = cbind( INP,  sn )
        }
      }
      # for space-time(year-averages)
      # st_vars = intersect( c( "tmean", "tsd", "tamplitude" ), p$stmv_variables$COV )
      newvars = setdiff(p$stmv_variables$COV, names(INP) )
      if (length(newvars) > 0) {
        sn = aegis_lookup( p=p, DS="spatial.annual", locsmap=locsmap, timestamp=INP[,"timestamp"], varnames=newvars )
        if (!is.null(sn)) {
        # colnames( sn  ) = st_vars
          if (ncol(sn) >0) INP = cbind( INP,  sn )
        }
      }

      tokeep = unique( c( p$stmv_variables$ALL, p$stmv_variables$Y, p$stmv_variables$LOCS ) )
      if (exists( "TIME", p$stmv_variables ) ) {
        tokeep = unique( c( tokeep, p$stmv_variables$TIME, "dyear", "yr" ) )
      }

      INP = INP[, which(names(INP) %in% tokeep ) ]  # a data frame
      oo = setdiff( c(p$stmv_variables$LOCS, p$stmv_variables$COV), names(INP))
      if (length(oo) > 0 ) {
        print(oo )
        stop("Some stmv_variables are missing in the input data")
      }
      INP = na.omit(INP)

    # cap quantiles of dependent vars
      if (exists("quantile_bounds", p)) {
        dr = list()
        for (vn in p$stmv_variables$COV) {
          dr[[vn]] = quantile( INP[,vn], probs=p$quantile_bounds, na.rm=TRUE ) # use 95%CI
          il = which( INP[,vn] < dr[[vn]][1] )
          if ( length(il) > 0 ) INP[il,vn] = dr[[vn]][1]
          iu = which( INP[,vn] > dr[[vn]][2] )
          if ( length(iu) > 0 ) INP[iu,vn] = dr[[vn]][2]
        }
      }

      PS = aegis_db( p=p, DS="prediction.surface" ) # a list object with static and annually varying stmv_variables
      PS = PS[ which(names(PS) %in% p$stmv_variables$COV ) ] # time vars, if they are part of the model will be created within stmv

      OUT = list( LOCS=bathymetry_db(p=p, DS="baseline"), COV=PS )

      return (list(input=INP, output=OUT))

    }


    # ------------------


    if ( DS %in% c("predictions", "predictions.redo" ) ) {
      # NOTE: the primary interpolated data were already created by stmv.
      # This routine points to this data and also creates
      # subsets of the data where required, determined by "spatial_domain_subareas"

      projectdir = file.path(p$data_root, "modelled", p$stmv_variables$Y, p$spatial_domain )

      if (DS %in% c("predictions")) {
        P = Pl = Pu = NULL
        fn = file.path( projectdir, paste("stmv.prediction", ret,  year, "rdata", sep=".") )
        if (file.exists(fn) ) load(fn)
        if (ret=="mean") return (P)
        if (ret=="lb") return( Pl)
        if (ret=="ub") return( Pu)
      }

      p0 = spatial_parameters( p=p ) # from
      L0 = bathymetry_db( p=p0, DS="baseline" )
      L0i = stmv::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
      sreg = setdiff( p$spatial_domain_subareas, p$spatial_domain )

      for (yr in p$yrs) {
        # downscale and warp from p(0) -> p1
        # print (yr)
        # default domain
        PP0 = stmv_db( p=p, DS="stmv.prediction", yr=yr, ret="mean")
        VV0 = stmv_db( p=p, DS="stmv.prediction", yr=yr, ret="lb")
        WW0 = stmv_db( p=p, DS="stmv.prediction", yr=yr, ret="ub")

        for ( gr in sreg ) {
          p1 = spatial_parameters( p=p, spatial_domain=gr ) # 'warping' from p -> p1
          L1 = bathymetry_db( p=p1, DS="baseline" )
          L1i = stmv::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
          L1 = planar2lonlat( L1, proj.type=p1$aegis_proj4string_planar_km )
          L1$plon_1 = L1$plon # store original coords
          L1$plat_1 = L1$plat
          L1 = lonlat2planar( L1, proj.type=p0$aegis_proj4string_planar_km )
          p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
            # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy
          P  = spatial_warp( PP0[], L0, L1, p0, p1, "fast", L0i, L1i )
          Pl = spatial_warp( VV0[], L0, L1, p0, p1, "fast", L0i, L1i )
          Pu = spatial_warp( WW0[], L0, L1, p0, p1, "fast", L0i, L1i )

          projectdir_p1 = file.path(p$data_root, "modelled", p1$stmv_variables$Y, p1$spatial_domain )
          dir.create( projectdir_p1, recursive=T, showWarnings=F )
          fn1_sg = file.path( projectdir_p1, paste("stmv.prediction.mean",  yr, "rdata", sep=".") )
          fn2_sg = file.path( projectdir_p1, paste("stmv.prediction.lb",  yr, "rdata", sep=".") )
          fn3_sg = file.path( projectdir_p1, paste("stmv.prediction.ub",  yr, "rdata", sep=".") )

          save( P, file=fn1_sg, compress=T )
          save( Pl, file=fn2_sg, compress=T )
          save( Pu, file=fn3_sg, compress=T )

          print (fn1_sg)
        }
      }

      if (0) {
        levelplot( P ~ plon_1 + plat_1, L1, aspect="iso", labels=FALSE, pretty=TRUE, xlab=NULL,ylab=NULL,scales=list(draw=FALSE) )
      }
    }

    #  -------------------------------

    if (DS %in% c(  "stmv.stats", "stmv.stats.redo" )){

      if (DS %in% c("stmv.stats")) {
        stats = NULL
        projectdir = file.path(p$data_root, "modelled", p$stmv_variables$Y, p$spatial_domain )
        fn = file.path( projectdir, paste( "stmv.statistics", "rdata", sep=".") )
        if (file.exists(fn) ) load(fn)
        return( stats )
      }

      # downscale and warp from p(0) -> p1
      # default domain
      S0 = stmv_db( p=p, DS="stmv.stats" )
      Snames = colnames(S0)
      p0 = spatial_parameters( p=p ) # from
      L0 = bathymetry_db( p=p0, DS="baseline" )
      L0i = stmv::array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
      sreg = setdiff( p$spatial_domain_subareas, p$spatial_domain )

      for ( gr in sreg ) {
        p1 = spatial_parameters( p=p, spatial_domain=gr ) # 'warping' from p -> p1
        L1 = bathymetry_db( p=p1, DS="baseline" )
        L1i = stmv::array_map( "xy->2", L1[, c("plon", "plat")], gridparams=p1$gridparams )
        L1 = planar2lonlat( L1, proj.type=p1$aegis_proj4string_planar_km )
        L1$plon_1 = L1$plon # store original coords
        L1$plat_1 = L1$plat
        L1 = lonlat2planar( L1, proj.type=p0$aegis_proj4string_planar_km )
        p1$wght = fields::setup.image.smooth( nrow=p1$nplons, ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres/3, xwidth=4*p1$pres, ywidth=4*p1$pres )
          # theta=p1$pres/3 assume at pres most of variance is accounted ... correct if dense pre-intepolated matrices .. if not can be noisy
        stats = matrix( NA, ncol=ncol(S0), nrow=nrow(L1) )
        for ( i in 1:ncol(S0) ) {
          stats[,i] = spatial_warp( S0[,i], L0, L1, p0, p1, "fast", L0i, L1i )
        }
        colnames(stats) = Snames
        projectdir_p1 = file.path(p$data_root, "modelled", p1$stmv_variables$Y, p1$spatial_domain )
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
      # assemble data for a given project

      if (DS=="complete") {
        IC = NULL
        projectdir = file.path(p$data_root, "modelled", p$stmv_variables$Y, p$spatial_domain )
        dir.create(projectdir, recursive=T, showWarnings=F)
        outfile =  file.path( projectdir, paste( "aegis", "complete", p$spatial_domain, "rdata", sep= ".") )
        if ( file.exists( outfile ) ) load( outfile )
        Inames = names(IC)
        if (is.null(varnames)) varnames=Inames
        varnames = intersect( Inames, varnames )
        if (length(varnames) == 0) varnames=Inames  # no match .. send all
        IC = IC[ , varnames]
        return(IC)
      }

      if (exists( "libs", p)) RLibrary( p$libs )

      grids = unique( c(p$spatial_domain_subareas , p$spatial_domain ) ) # operate upon every domain

      for (gr in grids ) {
        print(gr)

        p1 = spatial_parameters( p=p, spatial_domain=gr ) #target projection
        L1 = bathymetry_db(p=p1, DS="baseline")

        BS = aegis_db( p=p1, DS="stmv.stats" )
        colnames(BS) = paste(p1$stmv_variables$Y, colnames(BS), sep=".")
        IC = cbind( L1, BS )

        # climatology
        nL1 = nrow(L1)
        PS = PSlb = PSub = matrix( NA, nrow=nL1, ncol=p$ny )
        p1$stmvSaveDir = file.path(p1$data_root, "modelled", p1$stmv_variables$Y, p1$spatial_domain )


        for (iy in 1:p$ny) {
          yr = p$yrs[iy]
          oo=NULL;
          oo = aegis_db( p=p1, DS="predictions", year=yr, ret="mean" )
          if (!is.null(oo)) PS[,iy] = oo

          oo=NULL;
          oo = aegis_db( p=p1, DS="predictions", year=yr, ret="lb" )
          if (!is.null(oo)) PSlb[,iy] = oo

          oo=NULL;
          oo = aegis_db( p=p1, DS="predictions", year=yr, ret="ub" )
          if (!is.null(oo)) PSub[,iy] = oo
        }

        if (exists("quantile_bounds", p)) {
          qPS = quantile( PS, probs=p$quantile_bounds, na.rm=TRUE )
          u = which( PS < qPS[1])
          if (length(u)>0) PS[u] = qPS[1]
          v = which( PS > qPS[2])
          if (length(v)>0) PS[v] = qPS[2]

          qPSlb = quantile( PSlb, probs=p$quantile_bounds, na.rm=TRUE )
          u = which( PSlb < qPSlb[1])
          if (length(u)>0) PSlb[u] = qPSlb[1]
          v = which( PSlb > qPSlb[2])
          if (length(v)>0) PSlb[v] = qPSlb[2]

          qPSub = quantile( PSub, probs=p$quantile_bounds, na.rm=TRUE )
          u = which( PSub < qPSub[1])
          if (length(u)>0) PSub[u] = qPSub[1]
          v = which( PSub > qPSub[2])
          if (length(v)>0) PSub[v] = qPSub[2]
        }

        CL = cbind( apply( PS, 1, mean, na.rm=TRUE ),
                    apply( PSlb, 1, mean, na.rm=TRUE ),
                    apply( PSub, 1, mean, na.rm=TRUE ) )

        colnames(CL) = paste( p1$stmv_variables$Y, c("mean", "lb", "ub"), "climatology", sep=".")
        IC = cbind( IC, CL )
        PS = PSlb = PSub = NULL

        projectdir = file.path(p$data_root, "modelled", p1$stmv_variables$Y, p1$spatial_domain )
        dir.create( projectdir, recursive=T, showWarnings=F )
        outfile =  file.path( projectdir, paste( "aegis", "complete", p1$spatial_domain, "rdata", sep= ".") )
        save( IC, file=outfile, compress=T )
        print( outfile )

      }

      return( "Complete" )
    }

    # -------------------

    if (DS %in% c("baseline", "baseline.redo") ) {

      if ( DS=="baseline" ) {
        BL = list()
        for (vn in varnames ) {
          projectdir = file.path(p$data_root, "modelled", vn, p$spatial_domain )
          outfile =  file.path( projectdir, paste( "aegis", "baseline", p$spatial_domain, "rdata", sep= ".") )
          TS = NULL
          load( outfile)
          BL[[vn]] = TS
        }
        return (BL)
      }


      grids = unique( c(p$spatial_domain_subareas , p$spatial_domain ) ) # operate upon every domain

      for (gr in grids ) {
        # print(gr)
        p1 = spatial_parameters( p=p, spatial_domain=gr ) #target projection
        L1 = bathymetry_db(p=p1, DS="baseline")
        nL1 = nrow(L1)
        TS = matrix( NA, nrow=nL1, ncol=p$ny, dimnames=list(NULL, p$yrs) )
        for (i in 1:p$ny ) {
          yr = p$yrs[i]
          oo = aegis_db( p=p1, DS="predictions", year=yr, ret="mean" )
          if (!is.null(oo)) TS[,i] = oo
        }

        projectdir = file.path(p$data_root, "modelled", p1$stmv_variables$Y, p1$spatial_domain )
        dir.create( projectdir, recursive=T, showWarnings=F )
        outfile =  file.path( projectdir, paste( "aegis", "baseline", p1$spatial_domain, "rdata", sep= ".") )
        save( TS, file=outfile, compress=T )
        print( outfile )
      }

    }

 }
