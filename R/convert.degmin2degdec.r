
  convert.degmin2degdec = function (x, y=NULL, vnames=c("lon", "lat") ) {
    xlon = x[,vnames[1]]
    xlat = x[,vnames[2]]
    if (is.null(y)) {
      x[,vnames[1]] = floor(xlon) + round((xlon - floor(xlon)) /60 * 100, 6)
      x[,vnames[2]] = floor(xlat) + round((xlat - floor(xlat)) /60 * 100, 6)
    } else {
      if (y=="lon") x = - (floor(x/100)+(x-100*floor(x/100))/60)
      if (y=="lat") x = floor(x /100)+(x - 100*floor(x /100))/60
    }
    return (x)
  }


