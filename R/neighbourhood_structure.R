neighbourhood_structure = function( sppoly, strata_type="lattice" ) {

  if (strata_type == "lattice") {
    # generic lattice
    W.nb = poly2nb(sppoly, row.names=sppoly$StrataID, queen=TRUE)  # slow .. ~1hr?
    W.remove = which(card(W.nb) == 0)
    if ( length(W.remove) > 0 ) {
      # remove isolated locations and recreate sppoly .. alternatively add links to W.nb
      W.keep = which(card(W.nb) > 0)
      W.nb = nb_remove( W.nb, W.remove )
      sppoly = sppoly[W.keep,]
      row.names(sppoly) = as.character(sppoly$StrataID)
      sppoly = sp::spChFIDs( sppoly, row.names(sppoly) )  #fix id's
      sppoly$StrataID = factor( as.character(sppoly$StrataID) )
      sppoly$strata = as.numeric( sppoly$StrataID )
      sppoly = sppoly[order(sppoly$strata),]
      sppoly <<- sppoly  ## overwrite in parent data frame
    }
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }

  # ------------------------------------------------

  if (strata_type == "stratanal_polygons") {
    # predefined polygons with customized list of neighbours
    W.nb = spdep::poly2nb(sppoly, row.names=sppoly$StrataID, queen=TRUE )
    W.nb = maritimes_groundfish_strata( W.nb=W.nb, returntype="neighbourhoods" )  # customized neighbourshood structure
    attr(sppoly, "nb") = W.nb
    return( sppoly )
  }


}
