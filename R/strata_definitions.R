strata_definitions = function( id=NULL ) {
  message("\n NOTE: the function 'strata_definitions' has only a partial list .. it should be exanded by people that use this function... :)\n")
  strata=list()
  strata[["Gulf"]] = c("558", "559")
  strata[["Georges_Bank"]] = c("5Z1", "5Z2", "5Z3", "5Z4", "5Z8", "5Z7", "5Z6", "5Z5", "5Z9")
  strata[["Spring"]] = c("406", "401", "402", "400", "397", "399" , "398",  "404", "403",  "407", "405", "408" , "409",  "410", "411")
  strata[["Deep_Water"]] = c("503", "501", "502", "505", "504", "558", "559", "498", "497", "496")
  if (!is.null(id)) {
    out = NULL
    for (i in id) out = c( out, strata[[i]] )
    out = sort(unique( out))
    return(out)
  }
  return( strata )
}
