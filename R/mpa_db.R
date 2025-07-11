
  mpa_db = function( p=NULL, DS="polygons", project_to=NULL ) {

    message("deprecated? ...")

    mpadir = project.datadirectory( "aegis", "data")

    if (DS %in% c( "polygons.redo", "polygons" ) ) {
      fn = file.path( mpadir, "sab.polygons.rdz" )
      if (DS == "polygons" ) {
        out = NULL
        if  (file.exists( fn)) out = read_write_fast(fn)
          if ( !is.null(project_to)) {
            out$map.contours = st_transform(out$map.contours, st_crs(project_to))
            out$map.coastline = st_transform(out$map.coastline, st_crs(project_to))
            out$sab.polygons = st_transform(out$sab.polygons, st_crs(project_to))
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
      bbox = st_transform( bbox, st_crs(project_to) )

      aoi = aegis.polygons::polygon_db( polyid="StAnnsBank_AOI", project_to=project_to )
      z1 = aegis.polygons::polygon_db( polyid="StAnnsBank_Zone1", project_to=project_to  )
      z2 = aegis.polygons::polygon_db( polyid="StAnnsBank_Zone2", project_to=project_to  )
      z3 = aegis.polygons::polygon_db( polyid="StAnnsBank_Zone3", project_to=project_to  )
      z4 = aegis.polygons::polygon_db( polyid="StAnnsBank_Zone4", project_to=project_to   )

      out$sab.polygons = bind( aoi, z1, z2, z3, z4, keepnames=TRUE )

      mc = as( isobath_db(  DS="isobath", depths=p$map.depthcontours, project_to=project_to  ), "Spatial" )
      mcnames = names( mc)
      # must crop each one separately
      mcout = raster::crop( mc[1], bbox )
      for (i in 2:length(mc) ) {
        mcout = bind( mcout, raster::crop( mc[i], bbox ), keepnames=FALSE )
      }
      out$map.contours = as( isobath_db(  DS="isobath", depths=p$map.depthcontours, project_to=project_to  ), "Spatial" )

      # add a small buffer around data and clip to make smaller
      out$map.coastline = as( aegis.coastline::coastline_db( DS=" gshhg coastline highres redo ",
        xlim=p$corners$lon+c(-1,1), ylim=p$corners$lat+c(-1,1), no.clip=FALSE, level=1 ), "Spatial" )

      read_write_fast( out, file=fn )
      if ( !is.null(project_to)) {
        out$map.contours = st_transform(out$map.contours, st_crs(project_to))
        out$map.coastline = st_transform(out$map.coastline, st_crs(project_to))
        out$sab.polygons = st_transform(out$sab.polygons, st_crs(project_to))
      }
      return (out)
    }

  }


