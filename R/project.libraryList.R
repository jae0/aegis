

project.libraryList = function( DS="default" ) {

  if (DS %in% c("default" )  ){
    mylibs = data.frame( rbind(
      cbind( "aegis.base", file.path(code_root, "aegis.base"), "ecomod/aegis.base"),
      cbind( "aegis", file.path(code_root, "aegis"), "ecomod/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "ecomod/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "ecomod/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "ecomod/bio.taxonomy")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "bitbucketLoc" )
    return(mylibs)
  }

  if (DS %in% c("snowcrab" )  ){
    mylibs = data.frame( rbind(
      cbind( "aegis.base", file.path(code_root, "aegis.base"), "ecomod/aegis.base"),
      cbind( "aegis", file.path(code_root, "aegis"), "ecomod/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "ecomod/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "ecomod/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "ecomod/bio.taxonomy"),
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "ecomod/bio.snowcrab")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "bitbucketLoc" )
    return(mylibs)
  }

}

