

project.libraryList = function( DS="default" ) {

  if (DS %in% c("default" )  ){
    mylibs = data.frame( rbind(
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis", "ecomod/aegis"),
      cbind( "aegis.bathymetry", file.path(code_root, "aegis.bathymetry"), "jae0/aegis.bathymetry", "ecomod/aegis.bathymetry"),
      cbind( "aegis.coastline", file.path(code_root, "aegis.coastline"), "jae0/aegis.coastline", "ecomod/aegis.coastline"),
      cbind( "aegis.polygons", file.path(code_root, "aegis.polygons"), "jae0/aegis.polygons", "ecomod/aegis.polygons"),
      cbind( "aegis.substrate", file.path(code_root, "aegis.substrate"), "jae0/aegis.substrate", "ecomod/aegis.substrate"),
      cbind( "aegis.survey", file.path(code_root, "aegis.survey"), "jae0/aegis.survey", "ecomod/aegis.survey"),
      cbind( "aegis.temperature", file.path(code_root, "aegis.temperature"), "jae0/aegis.temperature", "ecomod/aegis.temperature"),
      cbind( "stmv", file.path(code_root, "stmv"), "jae0/stmv", "ecomod/aegis.stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" , "ecomod/netmensuration"),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy", "ecomod/bio.taxonomy")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "githubLoc", "bitbucketLoc" )
    return(mylibs)
  }

  if (DS %in% c("snowcrab" )  ){
    mylibs = data.frame(
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "jae0/bio.snowcrab", "ecomod/bio.snowcrab"), stringsAsFactors=FALSE)
    names(mylibs) = c( "libname", "gitLoc", "githubLoc", "bitbucketLoc" )
    default_libs = project.libraryList ( "default" )
    mylibs= rbind( default_libs, mylibs )
    return(mylibs)
  }

}

