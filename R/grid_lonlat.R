
  grid_lonlat = function (x, res=10) {
    multiplier = 100/res
    x = convert.degdec2degmin(x)
    x$lon = floor(x$lon / multiplier + 1L) * multiplier
    x$lat = floor(x$lat / multiplier + 1L) * multiplier
    x = convert.degmin2degdec(x)
    return(x)
  }
