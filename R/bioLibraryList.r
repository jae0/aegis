
bioLibraryList = function() {

  elibs = data.frame( rbind(
    cbind( "bio.base", "jae0/bio.base"),
    cbind( "bio.polygons", "jae0/bio.polygons"),
    cbind( "ecmei", "jae0/ecmei"),
    cbind( "bio.taxonomy", "jae0/bio.taxonomy"),
    cbind( "bio.utilities", "jae0/bio.utilities"),
    cbind( "bio.groundfish", "jae0/bio.groundfish"),
    cbind( "netmensuration", "jae0/netmensuration"),
    cbind( "bio.snowcrab", "jae0/bio.snowcrab")
  ), stringsAsFactors=FALSE)

  names(elibs) = c( "libname", "githubLoc" )
  return(elibs)
}


