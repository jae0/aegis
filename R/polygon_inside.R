
  polygon_inside = function( x, region, planar=FALSE, method="sp", 
    proj.type = "+proj=longlat +ellps=WGS84 +datum=WGS84 +units=km +no_defs",
    proj.lonlat = projection_proj4string("lonlat_wgs84")
    ) {

    # test if points are inside polygons, 
    # convenience wrapper for st_points_in_polygons() sf or sp point.in.polygon (fast)
    # using internal polygons (polygon_internal_code(), identified by name)
    
    region = aegis.polygons::polygon_internal_code( region )
      
    out = NULL

    if (method=="sp") {

      for (reg in region) {
        poly = data.table::fread( aegis.polygons::polygon_file(reg) )
        names(poly) =c("lon", "lat")

        a = NULL

        if (planar) {
          if (!exists("plon", x)) {
            if ("sf" %in% class(x)) {
              x = st_coordinates(x)
              names(x) = c("plon", "plat")
            }
          }
          poly.planar = lonlat2planar (poly, proj.type=proj.type)
          a = which(point.in.polygon(x$plon, x$plat, poly.planar$plon, poly.planar$plat) != 0 )
        }

        if (!planar) {
          if (!exists("lon", x)) {
            if ("sf" %in% class(x)) {
              x = st_coordinates(x)
              names(x) = c("lon", "lat")
            }
          }
          a = which(point.in.polygon(x$lon, x$lat, poly$lon, poly$lat) != 0 )
        }

        out = c(out, a)
      }
      out = sort(unique(out))
      return(out)

    }


    if (method =="sf") {

      require(sf)

   
      if (planar) {
        vns = c("plon", "plat")
        crs_internal = st_crs(proj.type)
      } else {
        vns = c("lon", "lat" )
        crs_internal = st_crs(proj.lonlat)
      } 
      
      ix = NULL
      if ("sf" %in% class(x) ) {
        pts = st_as_sf( x, crs=crs_internal )
      } else {
        ix = which(is.finite(x[[vns[1]]] ) & is.finite(x[[vns[2]]] ) )
        pts = st_as_sf( x[ ix, ..vns], coords=vns, crs=crs_internal )
        if (length(ix) ==0) {
          message("No positional information")
          return(NULL)
        }
      }
      
      polys = list()
      poly_vn=c("lon", "lat")
      for (reg in region) {
        poly = data.table::fread( aegis.polygons::polygon_file(reg) )
        names(poly) = poly_vn
        setDF(poly)
        if (planar) poly = lonlat2planar (poly, proj.type=proj.type)
        poly = st_as_sf( poly[, poly_vn], coords=poly_vn, crs=crs_internal )
        polys[[reg]] = st_cast(st_combine(poly), "POLYGON")
      }
      polys = st_sfc(sapply(polys, st_union), crs=crs_internal)


      # inside = which( st_points_in_polygons( pts = pts, polys = polys, method="sf_fast"  ) )
   
      # faster
      inside = which( st_points_in_polygons( pts = pts, polys = polys, method="sp_direct"  ) )
 
      if (!is.null(ix)) inside = ix[inside]
 
      return(inside)
    }


  }


