
 

  adjust.inflation = function( x, yr, reference.year, reverse=F) {
    cpi = aegis_ts_db( db="cpi", ref.year=reference.year )
    out = x * NA
    yrs = sort(unique(yr))
    for (YY in yrs) {
      adjustment = cpi$cpi[ which( cpi$yr == YY ) ]
      JJ = which( yr == YY )
      if (reverse) {
        out[JJ] = x[JJ] * adjustment
      } else {
        out[JJ] = x[JJ] / adjustment
      }
    }
    return (out)
  }



