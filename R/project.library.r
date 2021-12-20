project.library = function( mylibs = NULL, ... ) {

  if (is.null(mylibs)) {
    if (exists( "pkgs_local_git" )) {
      mylibs = data.frame( libname=pkgs_local_git  )
      mylibs$local = file.path(code_root, mylibs$libname) 
      mylibs$github = paste( "jae0", mylibs$libname, sep="/") 
      mylibs$bitbucket = paste( "ecomod", mylibs$libname, sep="/") 
    }
  }

  pkgsLoaded = .packages()
  pkgsInstalled = .packages(all.available = TRUE)

  ll = unique( c(...) )
  found = intersect( pkgsInstalled, ll )
  if (length(found) > 0 ) {
    for ( pkg in found ) require( pkg, character.only = TRUE )
  }

  notfound = setdiff( ll, pkgsInstalled )
  if (length(notfound) > 0) {
    print( "Missing some bio dependencies...")
    n = readline(prompt="Install them? (local/github/no): ")
    if (n %in% c("local", "github", "bitbucket") ) {
      if (exists("mylibs")) {
        for ( nf in notfound ) {
          oo = which( mylibs$libname == nf )
          if (n=="local") try( remotes::install_git( mylibs$git[oo[1]], dependencies=FALSE, ... ) )
          if (n=="github") try( remotes::install_github( mylibs$github [oo[1]], ... ) )
          if (n=="bitbucket") try( remotes::install_bitbucket( mylibs$bitbucket[oo[1]], ... ) )
          pkg = mylibs$libname[oo]
          require( pkg, character.only = TRUE )
        }
      }
    }
  }
  return( ll )
}



