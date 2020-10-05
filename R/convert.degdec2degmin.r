
  convert.degdec2degmin = function (x) {
    x$lat = aegis_floor(x$lat) + round(((x$lat - aegis_floor(x$lat))/100) * 60, 6)
    x$lon = aegis_floor(x$lon) + round(((x$lon - aegis_floor(x$lon))/100) * 60, 6)
    return (x)
  }



