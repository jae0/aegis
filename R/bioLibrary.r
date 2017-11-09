bioLibrary = function( ... ) {
  #\\ wrapper to load bio libraies from local installation
  #\\ or download/install and then load if missing

  ll = unique( c(...) )

  gref = bioLibraryList()
  pkgsLoaded = .packages()
  pkgsInstalled = .packages(all.available = TRUE)

  notinEcomod = setdiff( ll, gref$libname )
  if (length( notinEcomod) > 0 ) {
    print( "The following are not part of bio ... " )
    print( notinEcomod )
  }

  found = intersect( pkgsInstalled, ll )
  if (length(found) > 0 ) {
    for ( pkg in found ) {
      if ( pkg %in% pkgsLoaded ) {
        message("Reloading installed ", pkg )
        detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE )
      }
      require( pkg, character.only = TRUE )
    }
  }

  notfound = setdiff( ll, pkgsInstalled )
  if (length(notfound) > 0) {
    print( "Missing some bio dependencies...")
    n = readline(prompt="Install them? (y/n): ")
    if ( tolower(n) %in% c("y", "yes") ) {
      if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
      for ( nf in notfound ) {
        oo = which( gref$libname == nf )
        if (length(oo)==1) {
          require(devtools)
          try( install_github( gref$githubLoc[oo] ) )
          pkg = gref$libname[oo]
          if ( pkg %in% pkgsLoaded ) {
            message("Reloading installed ", pkg )
            detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE )
          }
          require( pkg, character.only = TRUE )
        }
      }
    }
  }
  return( ll )
}



