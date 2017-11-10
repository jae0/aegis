
bioLibraryList = function() {

  elibs = data.frame( rbind(
    cbind( "ecmenv", "jae0/ecmenv"),
    cbind( "ecmgis", "jae0/ecmgis"),
    cbind( "emei", "jae0/emei"),
    cbind( "bio.taxonomy", "jae0/bio.taxonomy"),
    cbind( "bio.utilities", "jae0/bio.utilities"),
    cbind( "bio.groundfish", "jae0/bio.groundfish"),
    cbind( "netmensuration", "jae0/netmensuration"),
    cbind( "bio.snowcrab", "jae0/bio.snowcrab")
  ), stringsAsFactors=FALSE)

  names(elibs) = c( "libname", "githubLoc" )
  return(elibs)
}


