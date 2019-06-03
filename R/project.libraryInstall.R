project.libraryInstall = function(source="local", DS="default", ...) {
  #\\ add install_github flags e.g. force=TRUE to call if desired
    pkgsInstalled = .packages(all.available = TRUE)
    if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
    mylibs = project.libraryList(DS=DS)
    if (source=="local") {
      for ( pkg in mylibs$gitLoc ) {
         try( devtools::install_git( pkg, dependencies=FALSE, ... ) )
      }
    } else if (source=="github") {
      for ( pkg in mylibs$githubLoc ) {
        try( devtools::install_github( pkg, ... ) )
      }
    }  else if (source=="bitbucket") {
      for ( pkg in mylibs$bitbucketLoc ) {
        try( devtools::install_bitbucket( pkg, ... ) )
      }
    }
}
