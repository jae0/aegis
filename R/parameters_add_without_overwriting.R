
parameters_add_without_overwriting = function( parameter_list, ... ) {
  to_add = list(...)
  vns = names( to_add )
  n = length( to_add )
  for ( i in 1:n) {
    a = vns[i]
    if ( !exists(a, parameter_list) )  parameter_list[[a]] = ...elt(i)
  }
  return (parameter_list)
}
