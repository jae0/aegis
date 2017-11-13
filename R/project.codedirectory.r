
project.codedirectory = function(...) {
  ## this function is required to bootstrap the other project level functions
  if (!exists("code_root")) stop("Define 'code_root' in your Rprofile...")  
  sep = .Platform$file.sep
  dirs = paste0( c(...) , collapse=sep )
  pd = file.path( code_root, dirs )
  pd = gsub( paste( sep, "$", sep=""), "", pd) # strip last path element
  if (! file.exists( pd)) {
    mk.Dir = readline(paste("Directory", pd, "not found, create it? Yes/No (Default No) : "))
    if ( mk.Dir =="Yes" ) {
      dir.create( pd, recursive=TRUE) 
      print( paste("The directory -- ",  pd, " -- has been created") )
    } else {
      warning( paste("Directory: ", pd, " was not found." )) 
    }
  }
  return ( pd )
}


