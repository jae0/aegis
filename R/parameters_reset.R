 
parameters_reset = function( parameter_list,
  to_reset= c(
      "project_name",
      "data_root", # reset data locations
      "datadir",  # reset data locations ( in aggregated_rawdata )
      "modeldir", 
      "variabletomodel",  # in case we need to run the modelling step on the fly  and data access
      "aegis_dimensionality", # in case we need to run modelling on the fly
      "areal_units_type",
      "areal_units_resolution_km",
      "areal_units_proj4string_planar_km",
      "areal_units_constraint",
      "areal_units_constraint_nmin",
      "areal_units_constraint_ntarget",
      "formula",  # in case we need to run the modelling step on the fly
      "family",  # in case we need to run the modelling step on the fly
      "carstm_model_label",  # to load data from carstm_summary
      "carstm_modelengine", 
      "inputdata_spatial_discretization_planar_km",
      "inputdata_temporal_discretization_yr",
      "data_transformation"   # in case we need to run modelling on the fly
    ) ) {

  # in a secondary call (from inside "carstm_inputs" ), spatial (grid or polygon info) and temporal structure information is passed to constrain results (areal_unit_fn, etc). However, the location of the information/base data required is in its own project-specific location, independent of the calling environment. These need to be reset.
  n = length( to_reset )
  pnames = names(p)

  for ( i in 1:n) {
    vn = to_reset[i]
    if ( vn %in% pnames ) parameter_list[[ vn ]] = NULL
  }
  return (parameter_list)
}
