
  polygon_inside = function( x, region, planar=FALSE, proj.type="+proj=longlat +ellps=WGS84 +datum=WGS84 +no_defs" ) {

    region = aegis.polygons::polygon_internal_code( region )
    out = NULL
    for (reg in region) {
      poly = data.table::fread( aegis.polygons::polygon_file(reg) )
      names(poly) =c("lon", "lat")

      a = NULL

      if (planar) {
        poly.planar = lonlat2planar (poly, proj.type=proj.type)
        a = which(point.in.polygon(x$plon, x$plat, poly.planar$plon, poly.planar$plat) != 0 )
      }

      if (!planar) {
        a = which(point.in.polygon(x$lon, x$lat, poly$lon, poly$lat) != 0 )
      }

      out = c(out, a)
    }
    out = sort(unique(out))
    return(out)
  }


