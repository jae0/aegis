list_to_dataframe = function( lst, method="clean" ) {
  if (method=="clean") return( do.call(rbind.data.frame, lst) )
  if (method=="transposed_fast") return(
    list(row.names=c(NA_integer_,length(lst)),
    class="data.frame", names=make.names(names(lst), unique=TRUE))
  )
}