

project.libraryList = function( DS="default" ) {

  if (DS %in% c("default" )  ){
    mylibs = data.frame( rbind(
      cbind( "aegis.base", file.path(code_root, "aegis.base"), "jae0/aegis.base"),
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "jae0/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "gitlabLoc" )
    return(mylibs)
  }

  if (DS %in% c("snowcrab" )  ){
    mylibs = data.frame( rbind(
      cbind( "aegis.base", file.path(code_root, "aegis.base"), "jae0/aegis.base"),
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "jae0/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy"),
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "jae0/bio.snowcrab")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "gitlabLoc" )
    return(mylibs)
  }
  
}

