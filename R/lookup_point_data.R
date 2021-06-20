

lookup_point_data = function( p, M, tolookup="bathymetry" ){

  # --------------------------

  setDF(M)
  
  if ("bathymetry" %in% tolookup) {
    require(aegis.bathymetry)

    pB = bathymetry_parameters( p=parameters_reset(p), project_class="carstm"  )
    vnB = pB$variabletomodel
    if ( !(exists(vnB, M ))) {
      vnB2 = paste(vnB, "mean", sep=".")
      if ((exists(vnB2, M ))) {
        names(M)[which(names(M) == vnB2 )] = vnB
      } else {
        M[,vnB] = NA
      }
    }
    iM = which(!is.finite( M[, vnB] ))
    if (length(iM > 0)) {
      M[iM, vnB] = bathymetry_lookup( LOCS=M[ iM, c("lon", "lat")],  lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data" ) # core=="rawdata"

    }

    M = M[ is.finite(M[ , vnB]  ) , ]

    if ( exists("spatial_domain", p)) {
        M = M[ geo_subset( spatial_domain=p$spatial_domain, Z=M ) , ] # need to be careful with extrapolation ...  filter depths
    }

    if ( p$carstm_inputdata_model_source$bathymetry %in% c("stmv", "hybrid") ) {
      pBD = bathymetry_parameters(  spatial_domain=p$spatial_domain, project_class=p$carstm_inputdata_model_source$bathymetry )  # full default
      LU = bathymetry_db( p=pBD, DS="baseline", varnames="all" )
      LU_map = array_map( "xy->1", LU[,c("plon","plat")], gridparams=p$gridparams )
      M_map  = array_map( "xy->1", M[, c("plon","plat")], gridparams=p$gridparams )
      iML = match( M_map, LU_map )
      vns = intersect(  c( "z", "dZ", "ddZ", "b.sdSpatial", "b.sdObs", "b.phi", "b.nu", "b.localrange" ), names(LU) )
      for (vn in setdiff( vns, "z") ) {
        M[, vn] = LU[ iML, vn ]
    }
      M = M[ is.finite( rowSums( M[ , vns])  ) , ]
    }
    return(M)
  }


  # --------------------------


  if ("substrate" %in% tolookup) {
    require(aegis.substrate)

    pS = substrate_parameters( p=parameters_reset(p), project_class="carstm"  )
    if (!(exists(pS$variabletomodel, M ))) M[,pS$variabletomodel] = NA
    iM = which(!is.finite( M[, pS$variabletomodel] ))
    if (length(iM > 0)) {
      M[iM, pS$variabletomodel] = substrate_lookup( LOCS=M[iM, c("lon", "lat")], lookup_from="core", lookup_to="points" , lookup_from_class="aggregated_data" ) # core=="rawdata"
    }

    M = M[ is.finite(M[ , pS$variabletomodel]  ) , ]
    return(M)
  }


  # --------------------------


  if ("temperature" %in% tolookup) {
    require(aegis.temperature)

    # temperature observations lookup
    pT = temperature_parameters( p=parameters_reset(p), project_class="carstm", year.assessment=p$year.assessment  )
    if (!(exists(pT$variabletomodel, M ))) M[,pT$variabletomodel] = NA
    iM = which(!is.finite( M[, pT$variabletomodel] ))
    if (length(iM > 0)) {
      M[iM, pT$variabletomodel] = temperature_lookup(  LOCS=M[ iM, c("lon", "lat", "timestamp")],lookup_from="core", lookup_to="points", lookup_from_class="aggregated_data", tz="America/Halifax",
          year.assessment=p$year.assessment
        )
    }


    M = M[ is.finite(M[ , pT$variabletomodel]  ) , ]
    M = M[ which( M[, pT$variabletomodel]  < 14 ) , ]  #
    return(M)
  }


  # --------------------------


  if ("speciescomposition" %in% tolookup) {
    require(aegis.temperature)

    pPC1 = speciescomposition_parameters( p=parameters_reset(p), project_class="carstm", variabletomodel="pca1" , year.assessment=p$year.assessment)
    if (!(exists(pPC1$variabletomodel, M ))) M[,pPC1$variabletomodel] = NA
    iM = which(!is.finite( M[, pPC1$variabletomodel] ))
    if (length(iM > 0)) {
      M[iM, pPC1$variabletomodel] = speciescomposition_lookup(  LOCS=M[ iM, c("lon", "lat", "timestamp")],lookup_from="core", lookup_to="points", lookup_from_class="aggregated_data", tz="America/Halifax" ,
          year.assessment=p$year.assessment ,
          vnames=pPC1$variabletomodel
        )

    }

    M = M[ which(is.finite(M[, pPC1$variabletomodel] )),]

    pPC2 = speciescomposition_parameters( p=parameters_reset(p), project_class="carstm", variabletomodel="pca2", year.assessment=p$year.assessment )
    if (!(exists(pPC2$variabletomodel, M ))) M[,pPC2$variabletomodel] = NA
    iM = which(!is.finite( M[, pPC2$variabletomodel] ))
    if (length(iM > 0)) {
      M[iM, pPC2$variabletomodel] = speciescomposition_lookup( LOCS=M[ iM, c("lon", "lat", "timestamp")], lookup_from="core", lookup_to="points", lookup_from_class="aggregated_data", tz="America/Halifax" ,
          year.assessment=p$year.assessment,
          vnames=pPC2$variabletomodel
        )

    }
    M = M[ which(is.finite(M[, pPC2$variabletomodel] )),]

    return(M)
  }


}