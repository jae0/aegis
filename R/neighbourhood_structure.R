neighbourhood_structure = function( sppoly, areal_units_type="lattice", snap=1 ) {

    # customized neighbourshood structure (hand edited)
  if (areal_units_type %in% c( "stratanal_polygons_pre2014", "stratanal_polygons", "groundfish_strata") ) {
    # predefined polygons with customized list of neighbours
    NB_graph = maritimes_groundfish_strata(  areal_units_timeperiod="pre2014" ,returntype="neighbourhoods" )  
    attr(sppoly, "nb") = NB_graph
    return( sppoly )
  }

  
  # customized neighbourshood structure (hand edited)
  if (areal_units_type %in% c( "stratanal_polygons_post2014" ) ) {
    # predefined polygons with customized list of neighbours
    NB_graph = maritimes_groundfish_strata(  areal_units_timeperiod="post2014" ,returntype="neighbourhoods" )  
    attr(sppoly, "nb") = NB_graph
    return( sppoly )
  }

  # ------------------------------------------------
  # otherwise, generic lattice, "tesselation", or "inla_mesh" 
   
    NB_graph = poly2nb(sppoly, row.names=sppoly$AUID, queen=TRUE, snap=snap )   
    NB_remove = which(card(NB_graph) == 0)
    if ( length(NB_remove) > 0 ) {
      # remove isolated locations and recreate sppoly .. alternatively add links to NB_graph
      NB_keep = which(card(NB_graph) > 0)
      NB_graph = nb_remove( NB_graph, NB_remove )
      sppoly = sppoly[NB_keep,]

      sppoly$AUID = as.character(sppoly$AUID)
      row.names(sppoly) = sppoly$AUID
      # sppoly = sp::spChFIDs( sppoly, row.names(sppoly) )  #fix id's
      sppoly = sppoly[order(sppoly$AUID),]
      sppoly <<- sppoly  ## overwrite in parent data frame
    }
    attr(sppoly, "nb") = NB_graph
    return( sppoly )
  
}
