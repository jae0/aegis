
aegis_prediction_surface = function( aegis_data, strata_subset=NULL ) {
  # convert list object as a tabular dataframe
  out = NULL
  vars = names(aegis_data)
  for ( vn in vars ) {
    if (is.matrix(aegis_data[[vn]])) {
      o = as.data.frame.table(aegis_data[[vn]], stringsAsFactors=FALSE)
      names(o) = c( "StrataID", "year", vn)
      if (is.null(out)) {
        out = o
      } else {
        out = merge(out, o, by=c("StrataID", "year"), all.x=TRUE, all.y=TRUE )
      }
    }
  }

  for ( vn in vars ) {
    if (is.vector(aegis_data[[vn]])) {
      o = data.frame( cbind(names(aegis_data[[vn]]), aegis_data[[vn]]), stringsAsFactors=FALSE)
      names(o) = c( "StrataID", vn)
      o[,vn] = as.numeric(o[,vn])
      if (is.null(out)) {
        out = o
      } else {
        out = merge(out, o, by=c("StrataID"), all.x=TRUE, all.y=TRUE )
      }
    }
  }

  if (!is.null(strata_subset)) {
    out = out[ which(as.character(out$StrataID)) %in% strata_subset, ]
  }

  return(out)
}
