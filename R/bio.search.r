
bio.search = function(  h="bio.help", ... ) {
  #// bio.search ( "keyword", ... )  -- '...' are grep options
  #// bio.search ( "^eco.*" )  -- keyword can be REGEX .. start a line with "eco"

  if ( h=="bio.help" ) {
    bio.help( "bio.search" )
    return("Pass keyword or a regular expression (?regex)")
  }

  fn.code = file.path( bio.workdirectory, "bio.help.sourcecode.rdata" )
  if ( !file.exists( fn.code) ) bio.help( "refresh" )
  load( fn.code )

    extractData = function(X, h, ... ) {
      ee = grep( h, X, ignore.case=TRUE, ... )
      out = NULL
      if (length(ee) >0 ) {
        out= paste(  paste( names(X), "[", ee, "]", sep=""), X[ee] )
      }
      return(out)
    }

  res = sapply( code, extractData, h=h, ... )
  rr = which( lapply( res, length ) > 0)

  if (length( rr) > 0 ) {
    fname = paste(tempfile(), "hmtl", sep=".")
    fn = file(fname, "w")
      cat( "\nFile names and [line number] where matches were found: \n", file=fn  )
      cat( "\n", file=fn  )
      for ( r in rr ) {
        output = res[r]
        cat( paste( names( output), ":" ), file=fn )
        cat("\n\n", file=fn )
        cat( paste(" ", unlist(output), sep="\n" ), file=fn  )
        cat("\n\n", file=fn )
      }
    close(fn)
    browseURL( fname )
  } else {
    cat( "No matches were found \n")
  }

}


