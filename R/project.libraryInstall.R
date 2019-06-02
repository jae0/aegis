project.libraryInstall = function(local=FALSE, DS="default", ...) {
  #\\ add install_gitlab flags e.g. force=TRUE to call if desired
    pkgsInstalled = .packages(all.available = TRUE)
    if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
    mylibs = project.libraryList(DS=DS)
    if (local) {
      for ( pkg in mylibs$gitLoc ) {
         try( devtools::install_git( pkg, dependencies=FALSE, ... ) )
      }
    } else {
      for ( pkg in mylibs$gitlabLoc ) {
        try( devtools::install_gitlab( pkg, ... ) )
      }
    }
}
