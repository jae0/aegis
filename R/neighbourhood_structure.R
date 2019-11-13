neighbourhood_structure = function( sppoly, areal_units_strata_type="lattice" ) {

  if (areal_units_strata_type == "lattice") {
    # generic lattice
    W.nb = poly2nb(sppoly, row.names=sppoly$AUID, queen=TRUE)  # slow .. ~1hr?
    W.remove = which(card(W.nb) == 0)
    if ( length(W.remove) > 0 ) {
      # remove isolated locations and recreate sppoly .. alternatively add links to W.nb
      W.keep = which(card(W.nb) > 0)
      W.nb = nb_remove( W.nb, W.remove )
      sppoly = sppoly[W.keep,]
      row.names(sppoly) = as.character(sppoly$AUID)
      sppoly = sp::spChFIDs( sppoly, row.names(sppoly) )  #fix id's
      sppoly$AUID = factor( as.character(sppoly$AUID) )
      sppoly$strata = as.numeric( sppoly$AUID )
      sppoly = sppoly[order(sppoly$strata),]
      sppoly <<- sppoly  ## overwrite in parent data frame
    }
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }

  # ------------------------------------------------

  if (areal_units_strata_type == "stratanal_polygons") {
    # predefined polygons with customized list of neighbours
    W.nb = spdep::poly2nb(sppoly, row.names=sppoly$AUID, queen=TRUE )
    W.nb = maritimes_groundfish_strata( W.nb=W.nb, returntype="neighbourhoods" )  # customized neighbourshood structure
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }


}
