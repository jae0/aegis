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
 
A more expanded version, similar to what I use, can be found below:

https://github.com/jae0/aegis/blob/master/inst/scripts/example_Rprofile.R

