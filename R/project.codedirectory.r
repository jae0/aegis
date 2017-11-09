

project.codedirectory = function(..., alternate.directory=NULL) {

  ## this function is required to bootstrap the other project level functions
  ## probably should make this cleaner one day .. :)

  # determine project directory string
  sep = .Platform$file.sep
  dirs = paste0( c(...) , collapse=sep )
  if (is.null(alternate.directory)) dirpath = bio.directory else dirpath = alternate.directory
  pd = file.path( dirpath, dirs )
  pd = gsub( paste( sep, "$", sep=""), "", pd) # strip last path element

  # nonexistant directories
  if (! file.exists( pd)) {
      warning( paste("Directory: ", pd, " was not found." ))
  }
  return ( pd )
}


