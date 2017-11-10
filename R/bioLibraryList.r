
bioLibraryList = function() {

  elibs = data.frame( rbind(
    cbind( "emenv", "jae0/emenv"),
    cbind( "emgis", "jae0/emgis"),
    cbind( "emei", "jae0/emei"),
    cbind( "bio.taxonomy", "jae0/bio.taxonomy"),
    cbind( "bio.groundfish", "jae0/bio.groundfish"),
    cbind( "netmensuration", "jae0/netmensuration"),
    cbind( "bio.snowcrab", "jae0/bio.snowcrab")
  ), stringsAsFactors=FALSE)

  names(elibs) = c( "libname", "githubLoc" )
  return(elibs)
}


