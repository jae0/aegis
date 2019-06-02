

  shannon.diversity = function(x, base=2, getid=TRUE) {
    rs = rowSums(x, na.rm=T)
    good = which(rs>0)
    if (length(good)==0) {
      if (getid) {
        out = rep(NA,4)
      } else {
        out = rep(NA,3)
      }
    }

    if (length(good)>0) {
      rs = rs[good]
      x = x[ good , ]
      id = rownames( x )
      pr = x / rs

      if (is.array(x) ) {
        shannon = - rowSums( pr * log( pr, base=base ), na.rm=T )
      } else {
        shannon = - sum( pr * log( pr, base=base ), na.rm=T )
      }

      Hmax = - log( rs, base=base )
      evenness = shannon / Hmax

      if (getid) {
        out = data.frame( id=id, shannon=shannon, evenness=evenness, Hmax=Hmax )
      } else {
        out = data.frame( shannon=shannon, evenness=evenness, Hmax=Hmax )
      }
    }

    return(out)
  }
