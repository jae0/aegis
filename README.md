Basic environment for using aegis*, bio*, and stm* packages. This provides the required functions to bootstrap and interact with each package.

To install you need to bootstrap from github directly: 

```
  devtools::install_github( "jae0/aegis.env" )
```

Then, you need to have an Rprofile set up properly such as:

```.
libPaths("~/R")
homedir = path.expand("~")
tmpdir = file.path( homedir, "tmp" )
work_root = file.path( homedir, "work" )    ### replace with correct path to work directory (local temporary storage)
code_root = file.path( homedir, "bio" )   ### replace with correct path to the parent directory of your git-projects
data_root = file.path( homedir, "bio.data" )   ### replace with correct path to your data

# store your passwords and login here and make sure they are secure
passwords = file.path( homedir, ".passwords" )
if (file.exists(passwords)) source( passwords )

require( aegis.env ) 
```


Thereafter, you can used the bootstrapped environment to install the other basic tools: 

```
  aegis.env::project.libraryInstall()
```

If you have a local git clone of the required packages, you can install with:

```
  aegis.env::project.libraryInstall(local=TRUE)  

```

For usage, examples can be found in https://github.com/jae0/aegis/tree/master/inst/scripts/. 

