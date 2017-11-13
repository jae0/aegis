

project.libraryList = function(project_root) {
    mylibs = data.frame( rbind(
      cbind( "LaplacesDemon", file.path(project_root, "LaplacesDemon"),"jae0/LaplacesDemon" ),
      cbind( "LaplacesDemonCpp", file.path(project_root, "LaplacesDemonCpp"), "jae0/LaplacesDemonCpp"),
      cbind( "emaf.base", file.path(project_root, "emaf.base"), "jae0/emaf.base"),
      cbind( "emaf", file.path(project_root, "emaf"), "jae0/emaf"),
      cbind( "stm", file.path(project_root, "stm"), "jae0/stm"),
      cbind( "netmensuration", file.path(project_root, "netmensuration"), "jae0/netmensuration" ),
      cbind( "bio.taxonomy", file.path(project_root, "bio.taxonomy"), "jae0/bio.taxonomy"),
      cbind( "bio.models", file.path(project_root, "bio.models"), "jae0/bio.models"),
      cbind( "bio.groundfish", file.path(project_root, "bio.groundfish"), "jae0/bio.groundfish"),
      cbind( "bio.snowcrab", file.path(project_root, "bio.snowcrab"), "jae0/bio.snowcrab")
    ), stringsAsFactors=FALSE)

    names(mylibs) = c( "libname", "gitLoc", "githubLoc" )
    return(mylibs)
}

