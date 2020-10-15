parameters_reset = function( parameter_list, ... ) {
  # in a secondary call (from inside "carstm_inputs" ), spatial (grid or polygon info) and temporal structure information is passed to constrain results (areal_unit_fn, etc). However, the location of the information/base data required is in its own project-specific location, independent of the calling environment. These need to be reset.
  to_reset = list(...)
  n = length( to_reset )
  if ( n == 0 ) {
    # default usage is in carstm ..
    vns = c( "data_root",  "datadir",  "carstm_model_call",  "carstm_model_tag",  "variabletomodel",  "aegis_dimensionality",  "data_transformation" )
    n = length(vns)
  } else {
    vns = names( to_reset )
  }

  for ( i in 1:n) {
    parameter_list[[vns[i]]] = NULL
  }

  return (parameter_list)
}
