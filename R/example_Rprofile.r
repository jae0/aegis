
if (0) {
# example Rprofile

options(  
  repos = c(CRAN = "http://mirror.its.dal.ca/cran/"),
  papersize = "letter",
  width = 120, 
  browser = "/usr/bin/flashpeak-slimjet",
  mc.cores = parallel::detectCores(), 
  # browserNLdisabled = TRUE, 
  deparse.max.lines = 2,
  prompt="R> ", 
  digits=4, 
  show.signif.stars=FALSE
)

Sys.setenv("PKG_CXXFLAGS"="-fopenmp -std=c++11")  # to permit openmp parallel build/funcitonaltity for rcpp
# Sys.setenv("OMP_NUM_THREADS"="1" )  # openmp thread count

.libPaths("~/R")  # define when you place your local libs

# determine home directory (homedir) 
if ( !exists("root.directory")) {
  if ( !exists("computer.system")) computer.system = Sys.info()["sysname"]
  if (computer.system=="Linux")   homedir = path.expand("~") #  linux
  if (computer.system=="Windows") homedir = file.path( "S:", "S & E files", "NATHprojects", "Projects" ) # Work
  if (computer.system=="Darwin")   homedir = file.path( "/Users", "xyz", "Documents", "Projects"  ) # MAC OS
}

tmpdir = file.path( homedir, "tmp")

# store your passwords and login here and make sure they are secure 
  passwords = file.path( homedir, ".passwords_file" )
  if (file.exists(passwords)) source( passwords ) 


# bio.* related things

 bio.workdirectory = file.path( homedir, "tmp" )     ### replace with correct path
 bio.directory = file.path( homedir, "bio" )   ### replace with correct path
 bio.datadirectory = file.path( homedir, "bio.data" )   ### replace with correct path

bc = bio.install = function(bio.dir="/home/jae/bio", ...) {
  mylibs = data.frame( rbind(
    cbind( "stmenv", file.path(bio.dir, "stmenv")),
    cbind( "stmdat", file.path(bio.dir, "stmdat")),
    cbind( "stm", file.path(bio.dir, "stm")),
    cbind( "bio.taxonomy", file.path(bio.dir, "bio.taxonomy")),
    cbind( "bio.models", file.path(bio.dir, "bio.models")),
    cbind( "bio.groundfish", file.path(bio.dir, "bio.groundfish")),
    cbind( "bio.snowcrab", file.path(bio.dir, "bio.snowcrab")),
    cbind( "netmensuration", file.path(bio.dir, "netmensuration")),
    cbind( "LaplacesDemon", file.path(bio.dir, "LaplacesDemon")),
    cbind( "LaplacesDemonCpp", file.path(bio.dir, "LaplacesDemonCpp"))
  ), stringsAsFactors=FALSE)

  names(mylibs) = c( "libname", "gitLoc" )

  #\\ add install_github flags e.g. force=TRUE to call if desired
  pkgsInstalled = .packages(all.available = TRUE)
  if ( ! "devtools" %in% pkgsInstalled ) install.packages( "devtools", dependencies=TRUE )
  require(devtools)
  for ( pkg in mylibs$gitLoc ) {
     try( install_git( pkg, dependencies=FALSE, ... ) )
  }
}



bio.install.github = function(...) {
  mylibs = c(
    "jae0/stmenv", 
    "jae0/stmdat", 
    "jae0/stm", 
    "jae0/LaplacesDemon", 
    "jae0/LaplacesDemonCpp", 
    "jae0/bio.taxonomy", 
    "jae0/bio.models", 
    "jae0/netmensuration", 
    "jae0/bio.groundfish", 
    "jae0/bio.snowcrab" )
    require(devtools)
    for ( pkg in mylibs ) try( install_github( pkg, ... ) )
}


  # To install ROracle: download from CRAN or Oracle:  ROracle_1.2-2.tar.gz and run something like:
  # R CMD INSTALL --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include' ROracle_1.2-2.tar.gz
  #


}
