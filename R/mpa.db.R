
  mpa.db = function( p=NULL, DS="polygons", project_to=NULL ) {

    mpadir = project.datadirectory( "aegis", "data")

    if (DS %in% c( "polygons.redo", "polygons" ) ) {
      fn = file.path( mpadir, "sab.polygons.rdata" )
      if (DS == "polygons" ) {
        out = NULL
        if  (file.exists( fn)) load(fn)
          if ( !is.null(project_to)) {
            out$map.contours = spTransform(out$map.contours, CRS(project_to))
            out$map.coastline = spTransform(out$map.coastline, CRS(project_to))
            out$sab.polygons = spTransform(out$sab.polygons, CRS(project_to))
          }
        return (out)
      }

      # storage/internal format is lon/lat:
      project_to = projection_proj4string("lonlat_wgs84")
      out = list()
      bbox = p$corners[ , c("lon", "lat")]
      colnames( bbox) = c("lon","lat")
      coordinates( bbox ) = c("lon", "lat")
      proj4string( bbox ) = project_to
      bbox = spTransform( bbox, CRS(project_to) )

      aoi = aegis.polygons::polygon.db( polyid="StAnnsBank_AOI", returnvalue="sp.polygon", project_to=project_to )
      z1 = aegis.polygons::polygon.db( polyid="StAnnsBank_Zone1", returnvalue="sp.polygon", project_to=project_to  )
      z2 = aegis.polygons::polygon.db( polyid="StAnnsBank_Zone2", returnvalue="sp.polygon", project_to=project_to  )
      z3 = aegis.polygons::polygon.db( polyid="StAnnsBank_Zone3", returnvalue="sp.polygon", project_to=project_to  )
      z4 = aegis.polygons::polygon.db( polyid="StAnnsBank_Zone4", returnvalue="sp.polygon", project_to=project_to   )

      out$sab.polygons = bind( aoi, z1, z2, z3, z4, keepnames=TRUE )

      mc = isobath.db( p=p, DS="isobath", depths=p$map.depthcontours, project_to=project_to  )
      mcnames = names( mc)
      # must crop each one separately
      mcout = raster::crop( mc[1], bbox )
      for (i in 2:length(mc) ) {
        mcout = bind( mcout, raster::crop( mc[i], bbox ), keepnames=FALSE )
      }
      out$map.contours = isobath.db( p=p, DS="isobath", depths=p$map.depthcontours, project_to=project_to  )

      # add a small buffer around data and clip to make smaller
      out$map.coastline = aegis.coastline::coastline_db( DS=" gshhg coastline highres redo ",
        xlim=p$corners$lon+c(-1,1), ylim=p$corners$lat+c(-1,1), no.clip=FALSE, level=1 )

      save( out, file=fn, compress=TRUE )
      if ( !is.null(project_to)) {
        out$map.contours = spTransform(out$map.contours, CRS(project_to))
        out$map.coastline = spTransform(out$map.coastline, CRS(project_to))
        out$sab.polygons = spTransform(out$sab.polygons, CRS(project_to))
      }
      return (out)
    }

  }


