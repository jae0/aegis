
stan_initialize = function( stan_code=NULL ) {
	
	required_libs = c( "cmdstanr", "posterior", "bayesplot" )

  pkgsInstalled = .packages(all.available = TRUE)
  missing_libs = setdiff( required_libs, pkgsInstalled )
	
  if (length(missing_libs) > 0 ) {
    repos = c("https://mc-stan.org/r-packages/", getOption("repos"))
		for ( lb in missing_libs )  install.packages( lb, repos = repos )
	}

	for ( rlib in required_libs ) require( rlib, character.only=TRUE )

	cmdstan_test = try( cmdstanr::cmdstan_path() )
	if ( inherits( cmdstan_test, "try-error" ) ) {
		install_cmdstan()  # installs into ~/.cmdstanr/
	} 

	message( "Using cmdsranr found at: ", cmdstan_path() )
	message( "version number: ", cmdstan_version() )
	
	if (!is.null(stan_code) ) {
		fn = write_stan_file( stan_code )
		stan_model = cmdstan_model( fn, compile=FALSE )
		return( stan_model )
	}

}

