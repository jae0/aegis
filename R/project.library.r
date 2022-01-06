project.library = function( ... ) {

  # same as RLibrary
  #\\ used to (re)load libraries conveniently

  ll = unique(c(...))

  pkgs = .packages(all.available = TRUE)
  pkgsLoaded = .packages()

  found = intersect( pkgs, ll )
  if (length(found) > 0 ) {
    for ( pkg in found ) {
      # try( detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE ), silent=TRUE )
      try ( suppressMessages( require( pkg, character.only = TRUE )), silent = TRUE  )
    }
  }

  notfound = setdiff( ll, pkgs )
  if (length(notfound) > 0) {
    print( "Missing some dependencies...")
    print( notfound )
    n = readline(prompt="Install them? (y/n): ")
    if ( tolower(n) %in% c("y", "yes") ) {
      for ( nf in notfound ) {
        try( utils::install.packages( nf, dependencies=TRUE ) )
        try( require( pkg, character.only = TRUE ) )
      }
    }
  }

  return( ll )
}



