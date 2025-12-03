
  polygon_inside = function( x, region, planar=FALSE, method="sp::point.in.polygon", 
    proj.type = "+proj=longlat +ellps=WGS84 +datum=WGS84 +units=km +no_defs",
    proj.lonlat = projection_proj4string("lonlat_wgs84")
    ) {

    # return row indices of points if inside a polygon  
    # convenience wrapper for st_points_in_polygons() sf or sp point.in.polygon (fast)
    # using internal polygons (polygon_internal_code(), identified by name)
    
    region = aegis.polygons::polygon_internal_code( region )
      
    out = NULL

    if (method=="sp::point.in.polygon") {
      
      # this is the fastest method and default
      if (inherits(x, "sf")) {
        x = st_coordinates(x)
        x = as.data.table(x)
        if (planar) {
          names(x) = c("plon", "plat")
        } else {
          names(x) = c("lon", "lat")
        }
      } else if (inherits(x, "Spatial")) {
        x = coordinates(x)
        x = as.data.table(x)
        if (planar) {
          names(x) = c("plon", "plat")
        } else {
          names(x) = c("lon", "lat")
        }
      } else {
        if (planar) {
          if (exists("plon", x)) {
            x = x[ , c("plon", "plat")]
          } else {
            names(x) = c("plon", "plat")
          }
        } else {
          if (exists("lon", x)) {
            x = x[, c("lon", "lat")]
          } else {
            names(x) = c("lon", "lat")
          }
        }
      }


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

    } else {
 
      # method="sf_fast" is reasonably fast

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
        poly = try(st_combine(poly))
        poly = st_cast(poly, "MULTILINESTRING")
        poly = st_simplify(poly )
        poly = st_cast(poly, "POLYGON")
        polys[[reg]] = poly
      }
      polys = st_sfc(sapply(polys, st_union), crs=crs_internal)
  
      inside = which( st_points_in_polygons( pts = pts, polys = polys, method=method ) )
 
      if (!is.null(ix)) inside = ix[inside]
 
      return(inside)
    }


  }


