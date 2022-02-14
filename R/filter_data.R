
filter_data = function( Y, selection ) {
  data.table::setDT(Y)
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
          tokeep = intersect( tokeep, which( Y[,..mm] >= rgs[1] & Y[,..mm] <= rgs[2] ) )
          next()
        }
      }
      if (exists( "less_than", selection)) {
        if ( mm %in% selection$less_than ) { # ranged variables
          dd =  selection[[mm]]
          tokeep = intersect( tokeep, which( Y[,..mm] < dd ) )
          next()
        }
      }
      if (exists( "greater_than", selection)) {
        if ( mm %in% selection$greater_than ) { # ranged variables
          dd =  selection[[mm]]
          tokeep = intersect( tokeep, which( Y[,..mm] > dd ) )
          next()
        }
      }
      tokeep = intersect( tokeep, which( Y[,..mm] %in% selection[[mm]] ) )  # exact matches otherwise as default
    }
  }
  return ( sort(unique(tokeep)) )
}
