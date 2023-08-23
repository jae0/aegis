# aegis

[aegis](https://github.com/jae0/aegis) is a R-library of utility functions that help interoperate with other aegis*, bio*, and stmv packages. They support data QA/QC, modelling and predicting with a focus upon spatial and spatiotemporal data manipulations.

If you do not want to deal with this as a package, you can load in functions to memory as:

```r
# download aegis to some local directory (=="savdir")

source( "https://github.com/jae0/aegis/blob/master/R/loadfunctions.r" )

loadfunctions("aegis", directory=savdir ) # read in the rest 

```


To install you run the following:

```r
  remotes::install_github( "jae0/aegis")
```

or you can add the following into your Rprofile:

```r
pkgsInstalled = .packages(all.available = TRUE)
if (!"remotes" %in% pkgsInstalled ) {
  o = try( utils::install.packages("remotes", dependencies=TRUE, ask=FALSE))
  if (inherits("try-error", o)) {
    remotes::install_github("cran/ROracle", args=" --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include'", force=TRUE)
  }
}

if (!"aegis" %in% pkgsInstalled ) {
  remotes::install_github( "jae0/aegis")  }
}
```

You probably will want to have an Rprofile set up properly such as:

```r
# libPaths("~/R")  # or where ever you like
homedir = path.expand("~")
tmpdir = file.path( homedir, "tmp" )
work_root = file.path( homedir, "work" )    ### replace with correct path to work directory (local temporary storage)
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

# store your passwords and login here and make sure they are secure
passwords = file.path( homedir, ".passwords" )
if (file.exists(passwords)) source( passwords )

require( aegis )
```
 

Here is a more expanded version of the Rprofile that I use:


```r

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

  cat("\n  .q = quit_without_saving()          # quit without saving\n" )
  cat("\n")
  cat("\n  .bio = load_bio_enviroment()        # load 'bio' environment" )
  cat("\n---\n")
}

.Last = function(){
  cat("\nSession ended at ", date(), "\n\n")
}



# ---- load bio specific libs:

# uncomment this out if you do want to load the environment automatically  
# -- this loads aegis, and any other libraries ..
# -- separation helps prevent race conditions when installing libraries

load_bio_enviroment = function() { try( source( file.path( code_root, "bio_startup.R" ) ) ) }  # various user-specific local options
makeActiveBinding(".bio", load_bio_enviroment, .GlobalEnv)

# .bio  

# or simply
suppressMessages( require( aegis ) )


```

And the "bio_startup.R" referenced at the end of the above section contains specific packages lists and other package management utilities to bring in all other aegis.* projects:

```r
# contents of my: bio_startup.R


# package management


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

gituser="jae0"
bitbucketuser="ecomod"


# ---

pkg_install_git = function( git_projects=git_projects_to_install, gituser=gituser, bitbucketuser=bitbucketuser, ...) {
  #\\ add install_github flags e.g. force=TRUE to call if desired
  require(remotes)
  cat( "\nUpdating ... \n" ) 

  for (i in 1:dim( git_projects )[1]) {
    lnm = git_projects$libname[i]
    lsc = git_projects$source[i]
    print( lnm ) 
    if ( lsc =="local" ){ 
      try( install_git( (file.path("file://", code_root, lnm)), dependencies=FALSE, upgrade=TRUE, ... ) )
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

#---
#
  cat("---\n\nShortcuts for bio-specific package management: \n")
  cat("\n  .pc = packages_clear()   # clear packages in memory" )
  cat("\n  .pk = pkg_update()       # update packages" )
  cat("\n  .pr = pkg_reinstall()    # reinstall all packages" )
  cat("\n  .pg = pkg_install_git()  # update git projects" )
  cat("\n\n---\n\n")

# specialized packages

pkgsInstalled = .packages(all.available = TRUE)

if (!"remotes" %in% pkgsInstalled ) {
  o = try( utils::install.packages("remotes", dependencies=TRUE, ask=FALSE))
  if (inherits("try-error", o)) {
        remotes::install_github("cran/ROracle", args=" --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include'", force=TRUE)
  }
}


if (!"INLA" %in% pkgsInstalled ) {
  utils::install.packages("INLA", repos=c(getOption("repos"), INLA="https://inla.r-inla-download.org/R/testing"), dep=TRUE)

  if (0) {
    require(INLA)
    
    inla_num.threads =  floor(parallel::detectCores()/2 )
    inla.setOption(num.threads=paste(inla_num.threads, ":2", sep="") )
    
    inla.setOption(mkl=FALSE)
    inla.setOption(pardiso.license="~/paradiso.license")
    inla.pardiso.check()
    # to force use, run with: control.compute=list(openmp.strategy="pardiso")
  }

}

if (!"aegis" %in% pkgsInstalled ) {
  message( "The package, aegis is missing. Install right now? (y/n):")
  ordl = readline()
  if (ordl=="y") remotes::install_github( "jae0/aegis")
}

suppressMessages( require( aegis ) )


if (0) {
  # defunct items

  # To install ROracle: download from CRAN or Oracle:  ROracle_1.2-2.tar.gz and run something like:
  # R CMD INSTALL --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include' ROracle_1.2-2.tar.gz
  # or,

  # https://cran.r-project.org/src/contrib/ROracle_1.3-1.tar.gz

  library(remotes)
  install_github("LaplacesDemonR/LaplacesDemon")
  install_github("lazycrazyowl/LaplacesDemonCpp")
  install_github("ecbrown/LaplacesDemon")  # older copy

  Sys.setenv("PKG_CXXFLAGS"="-fopenmp -std=c++11")  # to permit openmp parallel build/funcitonaltity for rcpp
  Sys.setenv("OMP_NUM_THREADS"="1" )  # openmp thread count
  Sys.setenv(USE_CXX14 = 1)
  invisible( Sys.setlocale("LC_COLLATE", "C") )  # turn off locale-specific sorting,

  Sys.setenv(DOWNLOAD_STATIC_LIBV8 = 1) # only necessary for Linux without the nodejs library / headers
  install.packages("rstan", repos = "https://cloud.r-project.org/", dependencies = TRUE)
  library("rstan") # observe startup messages
  .libPaths("~/R/x86_64-pc-linux-gnu-library/4.0/")

  # "posterior", "bayesplot",  # not working right now 8 jun 2021
  for (pkg in pkgs_reqd) if (! pkg %in% pkgsInstalled ) utils::install.packages( pkg, dependencies=TRUE, ask=FALSE)

  if (!"devtools" %in% pkgsInstalled ) utils::install.packages("devtools", dependencies=TRUE, ask=FALSE)
  if (!"posterior" %in% pkgsInstalled ) remotes::install_github("stan-dev/posterior")
  if (!"cmdstanr" %in% pkgsInstalled ) remotes::install_github("stan-dev/cmdstanr")
  if (!"ROracle" %in% pkgsInstalled ) remotes::install_github("cran/ROracle", args=" --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include'", force=TRUE)
  if (!"sf" %in% pkgsInstalled ) remotes::install_github("r-spatial/sf")


  # R magic

    # If your function is binary you can use an infix operator
    `%paste%` <- paste
    "a" %paste% "b"
    "repeat" <- toupper
    repeat "hello"

    hack <- function() {
      repeat {
        call_chr <- readline("> ")
        if(call_chr == "") break
        call <- as.call(lapply(strsplit(call_chr, " ")[[1]], as.name))
        res <- eval.parent(bquote(withVisible(.(call))))
        if(res$visible) print(res$value)
      }
      invisible(res$value)
    }

  # And since we decided we really hate parentheses we'll define an active binding so we can sneakily fire hack() just by typing .
    makeActiveBinding(".", hack, .GlobalEnv)
    x <- "a"
    y <- "b"
    # .
    #  paste x y

 }

```