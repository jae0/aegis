
# NOTE: use of makeActiveBinding in MSWindows may not work .. just comment them out

# directory/file paths
homedir = path.expand("~")

tmpdir = file.path( homedir, "tmp" )
work_root = file.path( homedir, "tmp" )		 ### replace with correct path
code_root = file.path( homedir, "bio" )   ### replace with correct path
data_root = file.path( homedir, "bio.data" )   ### replace with correct path

setwd( code_root )  # this is to make TAB-completion easier inside of R ...

passwords = file.path( homedir, ".passwords" )
if (file.exists(passwords)) {
  try( source( file.path( homedir, ".passwords" ) ) )  # could also just use REnviron for this ..
}

set.seed(12345)

## options

# ncols = Sys.getenv("COLUMNS")
# if (nzchar(ncols)) options(width = as.integer(ncols))
 
repositories = getOption("repos")

repositories["CRAN"] = "https://cloud.r-project.org/"
#repositories["CRAN"] = "http://mirror.its.dal.ca/cran/"

repositories["INLA"] = "https://inla.r-inla-download.org/R/testing"
#repositories["INLA"] = "https://inla.r-inla-download.org/R/stable"

# package management

gituser="jae0"
bitbucketuser="ecomod"


options(
  repos = repositories,
  papersize = "letter",
  # width = 110,
  browser = "/usr/bin/firefox",
  mc.cores = parallel::detectCores(),
  pdfviewer="zathura",
  deparse.max.lines = 2,
  prompt="R> ",
  digits=4,
  showWarnCalls=TRUE,
  showErrorCalls=TRUE,
  show.signif.stars=FALSE
)


close_all_graphics_windows = function(...) { graphics.off(...); invisible(gc()) }
makeActiveBinding(".xg", close_all_graphics_windows, .GlobalEnv)


quit_without_saving = function() .Internal(quit("no", 0, FALSE))
makeActiveBinding(".q", quit_without_saving, .GlobalEnv)
makeActiveBinding("q", quit_without_saving, .GlobalEnv)
 

# ---
libs0 = search()  # initial libs in memory
garbage_collect_libraries = function() {
  newobjs = setdiff( search(), libs0 )
  libs_new = newobjs[ grepl( "^package[:]", newobjs ) ] 
  # libs_new = gsub("package:", "", libs_new)
  if (length(libs_new)>0)  for ( pkg in libs_new) detach(pkg, character.only=TRUE )
  invisible( gc() )
}
makeActiveBinding(".xl", garbage_collect_libraries, .GlobalEnv)

# ---

vars0 = ls()
garbage_collect_memory = function() {
  newvars = setdiff( ls(), vars0 )
  if (length(newvars)>0)  for ( nv in newvars) rm(list=nv)
  invisible( gc() )
}
makeActiveBinding(".xm", garbage_collect_memory, .GlobalEnv)

# ---

environment_reinitialize = function() {
  close_all_graphics_windows() 
  garbage_collect_memory()
  garbage_collect_libraries()
  load_bio_enviroment()
}
makeActiveBinding(".xm", environment_reinitialize, .GlobalEnv)


# useful packages:
pkgs_reqd = unique( c(
  "akima", 
  "alphahull", 
  "bigmemory",  
  "colorspace",  
  "DBI", 
  "deSolve", 
  "grid",
  "GADMTools" , 
  "Hmisc", 
  "interp", 
  "FactoMineR",
  "fields", 
  "fftwtools", 
  "fftw", 
  "geosphere", 
  "GillespieSSA", 
  "googlesheets4", 
  "lubridate",  
  "lattice",
  "numDeriv", 
  "pacman",
  "RColorBrewer",
  "RandomFields", 
  "RandomFieldsUtils",
  "reshape2", 
  "splancs", 
  "SimInf", 
  "sf", 
  "stars",
  "term", 
  "tmap",
  "truncnorm",
  "vegan" 
) )
  
# "emdbook", "gstat",


# local git projects
pkgs_local_git = unique( c(
  "adapt",  
  "aegis",  
  "aegis.odemod",  
  "aegis.bathymetry",    
  "aegis.coastline",    
  "aegis.condition",    
  "aegis.metabolism",    
  "aegis.mpa",    
  "aegis.polygons",    
  "aegis.sizespectrum",    
  "aegis.substrate",    
  "aegis.survey",    
  "aegis.speciesarea",    
  "aegis.speciescomposition",    
  "aegis.temperature",    
  "bio.taxonomy",
  "bio.snowcrab",
  "carstm", 
  "ecomod",
  "netmensuration",  
  "stmv"
) )
git_projects_to_install = data.frame( libname=pkgs_local_git, source="local" )


# ---

pkg_install_git = function( git_projects=git_projects_to_install, gituser=gituser, bitbucketuser=bitbucketuser, ...) {
  #\\ add install_github flags e.g. force=TRUE to call if desired
  require(remotes)
  cat( "\nUpdating ... \n" ) 

  for (i in 1:dim( git_projects )[1]) {
    lnm = git_projects$libname[i]
    lsc = git_projects$source[i]
    cat( "\n---\n")
    cat( lnm )
    cat( "\n")
     
    if ( lsc =="local" ){ 
      try( install_git( (file.path("file:/", code_root, lnm)), dependencies=FALSE, upgrade=TRUE, ... ) )
    } else if (lsc =="github") {
      try( install_github( paste( gituser, lnm, sep="/") , ... ) ) 
    } else if (lsc =="bitbucket") {
      try( install_bitbucket( paste( bitbucketuser, lnm, sep="/"), ... ) )
    }
  }
}
makeActiveBinding(".pg", pkg_install_git, .GlobalEnv)



 
# ---


packages_clear = function( corepkgs = c("stats","graphics","grDevices","utils","datasets","methods", "base") ) {
# clear all packages from memory
  corepkgs = paste( "package", corepkgs, sep=":")  
  pkgs = search()
  pkgs = pkgs[ grep("package:",pkgs) ]
  pkgs = setdiff(pkgs, corepkgs)
  
  if (length(pkgs)>0)  for (package in pkgs) suppressMessages(invisible(try( detach(package, character.only=TRUE) )))

  # libs that seem to block reinstall 
  misbehaving_libs = paste( "package", c("dplyr", "sf", "vctrs"), sep=":")
  pkgs = search()
  pkgs = pkgs[ grep("package:",pkgs) ]
  pkgs = setdiff(pkgs, misbehaving_libs)
  pkgs = setdiff(pkgs, corepkgs)

  if (length(pkgs)>0) for (package in misbehaving_libs) suppressMessages(invisible(try( detach(package, character.only=TRUE) )))

  
  o = sessionInfo()
  if (exists("otherPkgs", o) ) {
    pkgs = paste("package", names(sessionInfo()$otherPkgs), sep=":") 
    suppressMessages(invisible(try( lapply(pkgs , detach, character.only = TRUE, unload = TRUE) )) )
  }
}
makeActiveBinding(".pc", packages_clear, .GlobalEnv)

#--
 
pkg_update = function(...) {
   packages_clear()
   update.packages( ask=FALSE, checkBuilt=TRUE )
}
makeActiveBinding(".pk", pkg_update, .GlobalEnv)

#---

pkg_reinstall = function(lib = .libPaths()[1], pkgs = as.data.frame(installed.packages(lib), stringsAsFactors=FALSE)$Package) {
   packages_clear()
   install.packages( lib=lib, pkgs=pkgs, type = 'source', dependencies=TRUE )
}
makeActiveBinding(".pr", pkg_reinstall, .GlobalEnv)



# ---

.First = function(){
  # make some functions operate without parentheses:
  # store your passwords and login here and make sure they are with secure permissisions
  cat("---\nSession started at", date(), "\n")

  cat("\nKey directories are as follows: \n")
  cat("\n  homedir = ", homedir )
  cat("\n  work_root = ", work_root )
  cat("\n  code_root = ", code_root )
  cat("\n  data_root = ", data_root )
  cat("\n")

  cat("\nShortcuts (enter command without parentheses):\n")
  cat("\n  .xg = close_all_graphics_windows()  # close all graphics windows" )
  cat("\n  .xm = garbage_collect_memory()      # clear data objects memory" )
  cat("\n  .xl = garbage_collect_libraries()   # clear libraries in memory" )
  cat("\n  .xe = environment_reinitialize()    # clear environment" )
  
  cat("\n  .pc = packages_clear()   # clear packages in memory" )
  cat("\n  .pk = pkg_update()       # update packages" )
  cat("\n  .pr = pkg_reinstall()    # reinstall all packages" )
  cat("\n  .pg = pkg_install_git()  # update git projects" )
 
  cat("\n  .q = quit_without_saving()          # quit without saving\n" )
  cat("\n")
  cat("\n  .bio = load_bio_enviroment()        # load 'bio' environment" )
  cat("\n---\n")
}

.Last = function(){
  cat("\nSession ended at ", date(), "\n\n")
}



# ---- load bio specific libs:

# in case aegis is not loaded, bootstrap here
try( source( system.file( "scripts", "aegis_startup.R", package = "aegis") ) )

# uncomment this out if you do want to load the environment automatically  
# -- this loads aegis, and any other libraries ..
# -- separation helps prevent race conditions when installing libraries

load_bio_enviroment = function() { try( source( file.path( code_root, "bio_startup.R" ) ) ) }  # various user-specific local options

makeActiveBinding(".bio", load_bio_enviroment, .GlobalEnv)

.bio  

