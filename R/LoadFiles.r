LoadFiles = function( ... ) {
  filelist = c(...)
  if ( length(filelist) > 0  ) for ( nm in c(...) ) source( file=nm )
  return( filelist )
}


