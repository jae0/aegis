
list_simply = function( Z, vn=NULL, returntype="data.frame" ){
  # list of lists to data frame
  if (!is.null(vn)) Z = Z[[vn]] 
  out = data.frame( lapply( Z, function(x) Reduce(c, x)))
  if ( returntype=="data.frame" )  return(out)
  if ( returntype=="data.table" )  return(as.data.table(out))
}