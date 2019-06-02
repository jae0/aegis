
  grid_lonlat = function (x, res=10) {
    multiplier = 100/res
    x = convert.degdec2degmin(x)
    x$lon = trunc(x$lon * multiplier) / multiplier
    x$lat = trunc(x$lat * multiplier) / multiplier
    x = convert.degmin2degdec(x)
    return(x)
  }
