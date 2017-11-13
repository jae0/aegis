if (0) {
    
    # example Rprofile

    options(  
      repos = c(CRAN = "http://mirror.its.dal.ca/cran/"),
      papersize = "letter",
      width = 120, 
      browser = "/usr/bin/flashpeak=slimjet",
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
      emaf_workdir = file.path( homedir, "tmp" )     ### replace with correct path
      project_root = file.path( homedir, "bio" )   ### replace with correct path
      data_root = file.path( homedir, "bio.data" )   ### replace with correct path

}
