bioLibraryClear = function(...) {
  #\\ clear a library from memory (namespace)
  #\\ used when developing code and there are conflicts in namespace
  pkgsLoaded = .packages()
  pkgsInstalled = .packages(all.available = TRUE)
  found = intersect( pkgsInstalled, c(...) )
  if (length(found) > 0 ) {
    for ( pkg in found ) {
      if ( pkg %in% pkgsLoaded ) {
        message("Removing package from memory (namespace): ", pkg )
        detach( paste("package", pkg, sep=":"), unload=TRUE, character.only=TRUE, force=TRUE )
      }
    }
  }
}


