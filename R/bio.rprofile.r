
# bio.directory
if(!exists("bio.directory")){
  print( "Please make sure define 'bio.directory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS)" )
}

# bio.workdirectory
if(!exists("bio.workdirectory")){
  print( "Please make sure define 'bio.workdirectory' in your Rprofile.site (Windows) or .Rprofile (Linux/MacOS)" )
  print( "Currently using: ")
  print ( getwd() )
} else {
	dir.create( bio.workdirectory, showWarnings = FALSE, recursive = TRUE )
  setwd( bio.workdirectory )
}

# load bootstrapping functions to finish initial setup
# bio.startupfiles = list.files( path=file.path( bio.directory, "_bioSetup" ), full.names=T, recursive=T,  ignore.case=T, include.dirs=F  )

# to prevent loop / race condition
# iprof = grep( "bio.rprofile.r", bio.startupfiles )

# for ( sf in bio.startupfiles[-iprof] ) source( sf )

# bio.startupfiles  ## this is a global variable

