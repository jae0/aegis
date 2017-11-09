bioLibraryInstall = function(...) {
  #\\ add install_github flags e.g. force=TRUE to call if desired
  pkgsInstalled = .packages(all.available = TRUE)
  if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
  require(devtools)
  gref = bioLibraryList()
  for ( pkg in gref$githubLoc ) {
    try( install_github( pkg, ... ) )
  }
}


