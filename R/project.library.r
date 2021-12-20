project.library = function( mylibs = NULL, ... ) {

  if (is.null(mylibs)) {
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

    names(mylibs) = c( "libname", "local", "github", "bitbucket" )
 
   }


  pkgsLoaded = .packages()
  pkgsInstalled = .packages(all.available = TRUE)

  ll = unique( c(...) )
  test = setdiff( ll, mylibs$libname )
  if (length( test) > 0 ) {
    # print( "The following are not part of the local projects ... " )
    # print( test )
  }

  found = intersect( pkgsInstalled, ll )
  if (length(found) > 0 ) {
    for ( pkg in found ) {
      # if ( pkg %in% pkgsLoaded ) {
      #   message("Reloading installed ", pkg )
      #   detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE )
      # }
      require( pkg, character.only = TRUE )
    }
  }

  notfound = setdiff( ll, pkgsInstalled )
  if (length(notfound) > 0) {
    print( "Missing some bio dependencies...")
    n = readline(prompt="Install them? (local/github/no): ")
    if (n %in% c("local", "github", "bitbucket") ) {
      for ( nf in notfound ) {
        oo = which( mylibs$libname == nf )
        if (n=="local") try( remotes::install_git( mylibs$git[oo[1]], dependencies=FALSE, ... ) )
        if (n=="github") try( remotes::install_github( mylibs$github [oo[1]], ... ) )
        if (n=="bitbucket") try( remotes::install_bitbucket( mylibs$bitbucket[oo[1]], ... ) )
        pkg = mylibs$libname[oo]
        # if ( pkg %in% pkgsLoaded ) {
        #   message("Reloading installed ", pkg )
        #   detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE )
        # }
        require( pkg, character.only = TRUE )
      }
    }
  }
  return( ll )
}



