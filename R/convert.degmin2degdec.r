
  convert.degmin2degdec = function (x, y=NULL, vnames=c("lon", "lat") ) {
    xlon = x[,vnames[1]]
    xlat = x[,vnames[2]]
    if (is.null(y)) {
      x[,vnames[1]] = aegis_floor(xlon) + round((xlon - aegis_floor(xlon)) /60 * 100, 6)
      x[,vnames[2]] = aegis_floor(xlat) + round((xlat - aegis_floor(xlat)) /60 * 100, 6)
    } else {
      if (y=="lon") x = - (aegis_floor(x/100)+(x-100*aegis_floor(x/100))/60)
      if (y=="lat") x = aegis_floor(x /100)+(x - 100*aegis_floor(x /100))/60
    }
    return (x)
  }


