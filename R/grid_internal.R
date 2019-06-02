
  grid_internal = function(x, gx) {
      # planar
      nx = length(gx)
      zx = c( gx, gx[nx]+(gx[2]-gx[1]) )
      X = as.numeric(as.character(cut( x, zx, include.lowest=T, right=F, labels=gx )))
      names(X) = names(x)
      return(X)
  }



