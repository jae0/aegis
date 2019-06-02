

  form.bins = function(base=2, x0, x1, length.out, log.transf=T) {

    if (log.transf) {
      lbound = log( x0, base=base )
      ubound = log( x1, base=base )
      i = seq(lbound, ubound, length.out=length.out+1)
      x = base^i
      nbins = length( x ) - 1
      lb = x[1: nbins]
      ub = x[2: (nbins+1)]
      mids = ( lb + ub )/2
      out = data.frame( mids=mids, lb=lb, ub=ub )
      out = log(out, base=base)

    } else {
      lbound = x0
      ubound = x1
      x = seq(lbound, ubound, length.out=length.out+1)
      nbins = length( x ) - 1
      lb = x[1: nbins]
      ub = x[2: (nbins+1)]
      mids = ( lb + ub )/2
      out = data.frame( mids=mids, lb=lb, ub=ub )
    }

    return(out)
  }


