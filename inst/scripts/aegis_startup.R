
Sys.chmod("0002")

pkgsInstalled = .packages(all.available = TRUE)

if (!"remotes" %in% pkgsInstalled ) {
  o = try( utils::install.packages("remotes", dependencies=TRUE, ask=FALSE))
  if (inherits("try-error", o)) {
        remotes::install_github("cran/ROracle", args=" --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include'", force=TRUE)
  }
}

if (!"aegis" %in% pkgsInstalled ) {
  message( "The package, aegis is missing. Install right now? (y/n):")
  ordl = readline()
  if (ordl=="y") remotes::install_github( "jae0/aegis")
}

suppressMessages( require( aegis ) )

if (0) {
  # note: INLA is a core part of aegis .. add something like this to your startup
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

  # other defunct items ..just examples of setting env vars to install fussy packages

  # To install ROracle: download from CRAN or Oracle:  ROracle_1.2-2.tar.gz and run something like:
  # R CMD INSTALL --configure-args='--with-oci-lib=/usr/lib --with-oci-inc=/usr/include' ROracle_1.2-2.tar.gz
  # or,

  # https://cran.r-project.org/src/contrib/ROracle_1.3-1.tar.gz

  library(remotes)
  install_github("LaplacesDemonR/LaplacesDemon")
 
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
