
aegis_prediction_surface = function( aegis_data, au_subset=NULL ) {
  # convert list object as a tabular dataframe
  out = NULL
  vars = names(aegis_data)
  for ( vn in vars ) {
    if (is.matrix(aegis_data[[vn]])) {
      o = as.data.frame.table(aegis_data[[vn]], stringsAsFactors=FALSE)
      names(o) = c( "AUID", "year", vn)
      if (is.null(out)) {
        out = o
      } else {
        out = merge(out, o, by=c("AUID", "year"), all.x=TRUE, all.y=TRUE )
      }
    }
  }

  for ( vn in vars ) {
    if (is.vector(aegis_data[[vn]])) {
      o = data.frame( cbind(names(aegis_data[[vn]]), aegis_data[[vn]]), stringsAsFactors=FALSE)
      names(o) = c( "AUID", vn)
      o[,vn] = as.numeric(o[,vn])
      if (is.null(out)) {
        out = o
      } else {
        out = merge(out, o, by=c("AUID"), all.x=TRUE, all.y=TRUE )
      }
    }
  }

  if (!is.null(au_subset)) {
    out = out[ which(as.character(out$AUID)) %in% au_subset, ]
  }

  return(out)
}
