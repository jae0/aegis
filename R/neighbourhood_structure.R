neighbourhood_structure = function( sppoly, areal_units_type="lattice" ) {

  
  # customized neighbourshood structure (hand edited)
  if (areal_units_type %in% c( "stratanal_polygons_pre2014", "stratanal_polygons", "groundfish_strata") ) {
    # predefined polygons with customized list of neighbours
    W.nb = maritimes_groundfish_strata(  areal_units_timeperiod="pre2014" ,returntype="neighbourhoods" )  
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }

  
  # customized neighbourshood structure (hand edited)
  if (areal_units_type %in% c( "stratanal_polygons_post2014" ) ) {
    # predefined polygons with customized list of neighbours
    W.nb = maritimes_groundfish_strata(  areal_units_timeperiod="post2014" ,returntype="neighbourhoods" )  
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }

  # ------------------------------------------------
  # otherwise, generic lattice, "tesselation", or "inla_mesh" 
  
    W.nb = poly2nb(sppoly, row.names=sppoly$AUID, queen=TRUE)  # slow .. ~1hr?
    W.remove = which(card(W.nb) == 0)
    if ( length(W.remove) > 0 ) {
      # remove isolated locations and recreate sppoly .. alternatively add links to W.nb
      W.keep = which(card(W.nb) > 0)
      W.nb = nb_remove( W.nb, W.remove )
      sppoly = sppoly[W.keep,]

      sppoly$AUID = as.character(sppoly$AUID)
      row.names(sppoly) = sppoly$AUID
      # sppoly = sp::spChFIDs( sppoly, row.names(sppoly) )  #fix id's
      sppoly = sppoly[order(sppoly$AUID),]
      sppoly <<- sppoly  ## overwrite in parent data frame
    }
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  
}
