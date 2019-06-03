project.library = function( ... ) {

  mylibs = project.libraryList()
  pkgsLoaded = .packages()
  pkgsInstalled = .packages(all.available = TRUE)

  ll = unique( c(...) )
  test = setdiff( ll, mylibs$libname )
  if (length( test) > 0 ) {
    print( "The following are not part of the local projects ... " )
    print( test )
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
    n = readline(prompt="Install them? (local/bitbucket/no): ")
    if (n %in% c("local", "bitbucket") ) {
      for ( nf in notfound ) {
        oo = which( mylibs$libname == nf )
        if (n=="local") try( devtools::install_git( mylibs$gitLoc[oo[1]], dependencies=FALSE, ... ) )
        if (n=="bitbucket") try( devtools::install_bitbucket( mylibs$bitbucketLoc[oo[1]], ... ) )
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



