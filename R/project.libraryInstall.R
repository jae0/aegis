project.libraryInstall = function(local=FALSE, ...) {
  #\\ add install_github flags e.g. force=TRUE to call if desired
    pkgsInstalled = .packages(all.available = TRUE)
    if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
    mylibs = project.libraryList()
    if (local) {
      for ( pkg in mylibs$gitLoc ) {
         try( devtools::install_git( pkg, dependencies=FALSE, ... ) )
      }
    } else {
      for ( pkg in mylibs$githubLoc ) {
        try( devtools::install_github( pkg, ... ) )
      }
    }
}
