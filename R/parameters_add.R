
parameters_add = function( param, param_arguments ) {
  if (is.null(param)) param=list()
  if (length(param_arguments) > 0 ) param = c(param, param_arguments)
  i = which(duplicated(names(param), fromLast = TRUE ))
  if ( length(i) > 0 ) param = param[-i] # give any passed parameters a higher priority, overwriting pre-existing variable
  return (param)
}
