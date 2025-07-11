
project.help = function( h="project.help", filepattern="\\.r$") {
  #// project.help( "aegis function name" ) - searches for help inside a function

  fn.docs = file.path( work_root, "project.help.docs.rdz")
  fn.code = file.path( work_root, "project.help.sourcecode.rdz" )

  if (h=="refresh") {
    #// project.help( "refresh" ) - refresh list of functions available locally
    print( "Refreshing locally available list of function names ... ")
    fl = NULL
    fl = list.files( path=code_root, pattern=filepattern, full.names=TRUE,
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
    read_write_fast( docs, file=fn.docs )
    read_write_fast( code, file=fn.code )
    print( paste( "Local help files saved to ", work_root ))
  }

  if ( !file.exists( fn.docs) | !file.exists( fn.code) ) project.help( "refresh" )
  docs = read_write_fast( fn.docs )

  mm = grep( h, names( docs), ignore.case=TRUE )
  if (length(mm) == 0) return( "Function not found. Try refreshing local help with: project.help('refresh')) ")

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


