

project.libraryList = function() {
    mylibs = data.frame( rbind(
      cbind( "LaplacesDemon", file.path(code_root, "LaplacesDemon"),"jae0/LaplacesDemon" ),
      cbind( "LaplacesDemonCpp", file.path(code_root, "LaplacesDemonCpp"), "jae0/LaplacesDemonCpp"),
      cbind( "aegis.env", file.path(code_root, "aegis.env"), "jae0/aegis.env"),
      cbind( "aegis", file.path(code_root, "aegis"), "jae0/aegis"),
      cbind( "stm", file.path(code_root, "stm"), "jae0/stm"),
      cbind( "netmensuration", file.path(code_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(code_root, "bio.taxonomy"), "jae0/bio.taxonomy"),
      cbind( "bio.models", file.path(code_root, "bio.models"), "jae0/bio.models"),
      cbind( "bio.snowcrab", file.path(code_root, "bio.snowcrab"), "jae0/bio.snowcrab")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "githubLoc" )
    return(mylibs)
}
