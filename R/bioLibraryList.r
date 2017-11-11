
bioLibraryList = function() {

  elibs = data.frame( rbind(
    cbind( "stmenv", "jae0/stmenv"),
    cbind( "stmdat", "jae0/stmdat"),
    cbind( "stm", "jae0/stm"),
    cbind( "bio.taxonomy", "jae0/bio.taxonomy"),
    cbind( "bio.groundfish", "jae0/bio.groundfish"),
    cbind( "netmensuration", "jae0/netmensuration"),
    cbind( "bio.snowcrab", "jae0/bio.snowcrab")
  ), stringsAsFactors=FALSE)

  names(elibs) = c( "libname", "githubLoc" )
  return(elibs)
}


