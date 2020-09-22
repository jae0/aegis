
  convert.degdec2degmin = function (x) {
    x$lat = floor(x$lat) + round(((x$lat - floor(x$lat))/100) * 60, 6)
    x$lon = floor(x$lon) + round(((x$lon - floor(x$lon))/100) * 60, 6)
    return (x)
  }



