
discretize_data = function( x=NULL, brks=NULL, span=NULL, additional_digits=3,
  labels=NULL, digits=NULL, resolution=NULL, truncation_factor=0.1, toreturn="midpoints" ) {
  
	# discretize data x along the break points .. and midpoints as labels, with signif digits digits
  
  if (!is.null(span) & is.null(brks)) {
    # create sequence:
    brks = seq( span[1], span[2], length.out=span[3]+1) 
  }

  if (is.null(x) ) {
    if (!is.null(span)) {
      if (toreturn=="lower") {
        # drop the right most
        brks = brks[-length(brks)]
        return(brks)  
      }
      if (toreturn=="upper") {
        # drop the left most
        brks = brks[-1]
        return(brks)  
      }
    }
  }

  brks = sort(brks)
  dx = abs(diff(brks))
  
  if (is.null(digits)) digits = ceiling(log10(1/min(dx))) + additional_digits  # three more than leading digit (ie ~ 4 significant digits)
  if (is.null(resolution)) resolution = 1/10^digits
  
  # truncate data to break limits .. extreme being one increment lower/higher
  
  if (is.null(labels)) {
    labels = c(dx, dx[length(dx)]) / 2 + brks  # midpoints
    labels = trunc(labels / resolution)*resolution
    labels = round(labels, digits=digits )
  }

  if (is.null(x)) {
    if ( toreturn=="midpoints") {
      if ( !is.null(span)) {
        return( labels[-length(labels)] )
      } else {
        return( labels  )
      }
    }
  }

  if (!is.null(x)) {

    ii = which( x <= min(brks) ) 
    if (length(ii) > 0) {
      x[ii] = min(brks) + dx[1] * truncation_factor
    } 
    
    jj = which( x >= max(brks) ) 
    if (length(jj) > 0) {  
      x[jj] = max(brks) - dx[length(dx)] * truncation_factor
    }
    
    o = as.numeric( as.character( cut( 
      x, 
      breaks=brks, 
      labels=labels[-length(labels)], 
      include.lowest=TRUE ) 
    ))
     
    return( o )
  }

  return( message("Error .. check your arguments?"))
}
