

project.datadirectory = function(...) {
  if (!exists("data_root")) stop("Define 'data_root' in your Rprofile...")  
  sep = .Platform$file.sep
  dirs = paste0( c(...) , collapse=sep ) 
  pd = file.path( data_root, dirs )
  pd = gsub( paste( sep, "$", sep=""), "", pd) # strip last path element 
  if (! file.exists( pd)) {
    # mk.Dir = readline(paste("Directory", pd, "not found, create it? Yes/No (Default No) : "))
    # if ( mk.Dir =="Yes" ) {
      dir.create( pd, recursive=TRUE) 
      print( paste("The directory -- ",  pd, " -- has been created") )
    # } else {
    #  warning( paste("Directory: ", pd, " was not found." )) 
    # }
  }
  return ( pd )
}


