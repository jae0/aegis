
bgit = function( project="bio.base", action="status", ... ) {
  #\\ interact with bio.* tools to interpoate with git
  # NOTE:: default is to return to branch "develop"

  wd.start = getwd()

  reploc = bio.base::project.codedirectory( project )
  setwd( reploc )

  if (action=="status" ) {
    system2( "git",  "status" )
  }

  if (action=="commit" ) {
    if ( length(c(...))==0 ) stop( "Need to send a comment as a third element for the commit.")
    system2( "git",  paste("commit -am '", c(...), "'") )
  }

  if (action=="checkout" ) {
    system2( "git",  paste("checkout", c(...)) )
  }

  if (action=="branch" ) {
    system2( "git",  paste("branch", c(...)) )
  }

  if (action=="merge" ) {
    system2( "git",  paste("merge", c(...)) )
  }

  if (action=="merge.develop.to.master" ) {
    system2( "git", "checkout master" )
    system2( "git", "merge develop" )
    system2( "git", "checkout develop" )
  }

  if (action=="merge.master.to.develop" ) {
    system2( "git", "checkout develop" )
    system2( "git", "merge master" )
  }

  if (action=="pull" ) {
    system2( "git", "checkout master" )
    system2( "git", paste( "pull", c(...) ) )
    system2( "git", "checkout develop" )
   }

  if (action=="push" ) {
    system2( "git", "checkout master" )
    system2( "git", paste( "push", c(...) ) )
    system2( "git", "checkout develop" )
   }


  if (action=="push.to.github" ) {
    system2( "git", "checkout master" )
    system2( "git", "push" )
    system2( "git", "checkout develop" )
  }

  if (action=="update.github" ) {
    if ( length(c(...))==0 ) stop( "Need to send a comment as a third element for the commit.")
    system2( "git",  paste("commit -am '", c(...), "'" ) )
    system2( "git", "checkout master" )
    system2( "git", "pull" )
    system2( "git", "merge develop" )
    system2( "git", "push" )
    system2( "git", "checkout develop" )
    system2( "git", "merge master" )
  }

  if (action=="update" ) {
    if ( length(c(...))==0 ) stop( "Need to send a comment as a third element for the commit.")
    system2( "git",  paste("commit -am '", c(...), "'" ) )
    system2( "git", "checkout master" )
    system2( "git", "merge develop" )
    system2( "git", "checkout develop" )
  }

  if (action=="direct" ) {
    system2( "git", c(...) )
  }

  if (action!="branch" ) system2( "git", "branch") # conditional in case it gets called twice
  system2( "git", "status")
  setwd( wd.start )
  print( paste0( "Returning working directory to initial location: ", wd.start ) )

  invisible()
}



