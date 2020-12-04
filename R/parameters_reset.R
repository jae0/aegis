parameters_reset = function( parameter_list,
  to_reset= c(
      "project_name",
      "data_root", # reset data locations
      "datadir",  # reset data locations ( in aggregated_rawdata )
      "modeldir",
      "variabletomodel",  # in case we need to run the modelling step on the fly  and data access
      "carstm_model_call",  # in case we need to run the modelling step on the fly
      "aegis_dimensionality", # in case we need to run modelling on the fly
      "carstm_model_label",  # to load data from carstm_summary
      "data_transformation"   # in case we need to run modelling on the fly
    ) ) {

  # in a secondary call (from inside "carstm_inputs" ), spatial (grid or polygon info) and temporal structure information is passed to constrain results (areal_unit_fn, etc). However, the location of the information/base data required is in its own project-specific location, independent of the calling environment. These need to be reset.
  n = length( to_reset )

  for ( i in 1:n) parameter_list[[to_reset[i]]] = NULL

  return (parameter_list)
}
