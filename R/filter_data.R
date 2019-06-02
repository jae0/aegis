
filter_data = function( Y, selection ) {
  tokeep = 1:nrow(Y)
  nm = names(selection)
  nn = length(nm)
  nY = names(Y)
  for (i in 1:nn ) {
    mm = nm[i]
    if (mm %in% nY ) {
      if (exists( "ranged_data", selection)) {
        if ( mm %in% selection$ranged_data ) { # ranged variables
          rgs = range( selection[[mm]], na.rm=TRUE)
          tokeep = intersect( tokeep, which( Y[,mm] >= rgs[1] & Y[,mm] <= rgs[2] ) )
          next()
        }
      }
      tokeep = intersect( tokeep, which( Y[,mm] %in% selection[[mm]] ) )  # exact matches otherwise as default
    }
  }
  return ( sort(unique(tokeep)) )
}
