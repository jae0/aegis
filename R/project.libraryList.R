

project.libraryList = function( DS="default" ) {
  if (DS=="default") {
    mylibs = data.frame( rbind(
      cbind( "aegis.env", file.path(code_root, "aegis.env"), "jae0/aegis.env"),
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "jae0/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy"),
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "jae0/bio.snowcrab")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "githubLoc" )
    return(mylibs)
  }
  
  if (DS=="jae") {
    mylibs = data.frame( rbind(
      cbind( "LaplacesDemon", file.path(code_root, "LaplacesDemon"),"jae0/LaplacesDemon" ),
      cbind( "LaplacesDemonCpp", file.path(code_root, "LaplacesDemonCpp"), "jae0/LaplacesDemonCpp"),
      cbind( "aegis.env", file.path(code_root, "aegis.env"), "jae0/aegis.env"),
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis"),
      cbind( "stmv", file.path(code_root, "stmv"), "jae0/stmv"),
      cbind( "stmr", file.path(code_root, "stmr"), "jae0/stmv"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy"),
      cbind( "bio.models", file.path(code_root, "bio.models"), "jae0/bio.models"),
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "jae0/bio.snowcrab")
    ), stringsAsFactors=FALSE)
    names(mylibs) = c( "libname", "gitLoc", "githubLoc" )
    return(mylibs)
  }
  
}

