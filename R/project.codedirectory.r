
project.codedirectory = function(...) {
  ## this function is required to bootstrap the other project level functions
  if (!exists("code_root")) stop("Define 'code_root' in your Rprofile...")  
  sep = .Platform$file.sep
  dirs = paste0( c(...) , collapse=sep )
  pd = file.path( code_root, dirs )
  pd = gsub( paste( sep, "$", sep=""), "", pd) # strip last path element
  if (! file.exists( pd)) {
    stop (paste("Directory", pd, "not found. "))
  }
  return ( pd )
}


