
bio.help = function( h="bio.help", filepattern="\\.r$") {
  #// bio.help( "bio function name" ) - searches for help inside a function

  fn.docs = file.path( bio.workdirectory, "bio.help.docs.rdata")
  fn.code = file.path( bio.workdirectory, "bio.help.sourcecode.rdata" )

  if (h=="refresh") {
    #// bio.help( "refresh" ) - refresh list of bio functions available locally
    print( "Refreshing locally available list of function names ... ")
    fl = NULL
    fl = list.files( path=bio.directory, pattern=filepattern, full.names=TRUE,
                     recursive=TRUE,  ignore.case=TRUE, include.dirs=FALSE )
    code = list()
    docs = list()
    for ( f in fl ) {
      o = scan( f, "character", sep="\n", quiet=TRUE )
      k = try( grep ( "\\#('|\\/\\/)", o, ignore.case=TRUE  ), silent=TRUE )
      res = "No help .. please preface some comments with #// or #' to add some"
      if (! (class(k) %in% "try-error")) {
        if ( length(k) > 0) {
          res = gsub( "^[[:space:]]*\\#('|\\/\\/)[[:space:]]*", "", o[k] )
          res = list( res )
      }}
      docs[f] = res
      code[f] = list( o )
    }
    save( docs, file=fn.docs, compress=TRUE )
    save( code, file=fn.code, compress=TRUE )
    print( paste( "Local help files saved to ", bio.workdirectory ))
  }

  if ( !file.exists( fn.docs) | !file.exists( fn.code) ) bio.help( "refresh" )
  load( fn.docs )

  mm = grep( h, names( docs), ignore.case=TRUE )
  if (length(mm) == 0) return( "Function not found. Try refreshing local help with: bio.help('refresh')) ")

  fname = paste(tempfile(), "hmtl", sep=".")
  fn = file(fname, "w")
    cat("\n", file=fn )
    cat( "Matches found at:", file=fn  )

    for ( m in mm ) {
      cat( "\n", file=fn  )
      cat( "\n", file=fn  )
      output = docs[m]
      cat( paste( names( output), ":" ), file=fn )
      cat("\n", file=fn )
      cat( paste(" ", unlist(output), sep="\n" ), file=fn  )
      cat( "\n", file=fn  )
    }

  close(fn)

  browseURL( fname )
}


