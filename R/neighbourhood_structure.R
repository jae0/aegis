neighbourhood_structure = function( sppoly, areal_units_source="lattice" ) {

  if (areal_units_source == "lattice") {
    # generic lattice
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

  # ------------------------------------------------

  if (areal_units_source == "stratanal_polygons") {
    # predefined polygons with customized list of neighbours
    W.nb = spdep::poly2nb(sppoly, row.names=sppoly$AUID, queen=TRUE )
    W.nb = maritimes_groundfish_strata( W.nb=W.nb, returntype="neighbourhoods" )  # customized neighbourshood structure
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }


}
