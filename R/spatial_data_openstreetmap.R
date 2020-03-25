data_spatial_openstreetmap = function( DS, p, redo=FALSE, fn=NULL, ... ) {

  # testing open street maps data ...

  p = parameters_control(p, list(...), control="add") # add passed args to parameter list, priority to args


  # -----------------------------

    if (DS=="NS_coastline_openstreetmap") {
    # https://planet.openstreetmap.org/  https://planet.openstreetmap.org/historical-shapefiles/processed_p.tar.bz2 (projection not clear)
    # http://data.openstreetmapdata.com/coastlines-split-4326.zip (WGS84)

      fn = file.path( p$data.directory, "polygons", "NS_coastline_openstreetmap.rdata" )
      if ( !redo ) {
        if ( file.exists(fn) )  {
        load( fn )
        if (exists("aegis_proj4string_planar_km", p)) pg = spTransform( pg, sp::CRS(p$aegis_proj4string_planar_km) )
        return (pg)
      }
      }

      message( "Downloading .. you will need to manully unzip this one." )
      fn1 = "http://data.openstreetmapdata.com/land-polygons-complete-4326.zip"
      dir.local = file.path( p$data.directory, "polygons", "openstreetmap", "coastlines-split-4326" )
      dir.create( dir.local, recursive=TRUE, showWarnings=FALSE )
      fn.local = file.path( dir.local, basename( fn1 ) )
      shapefilename_osm = file.path( dir.local, "land_polygons.shp" )

      download.file( url=fn1, destfile=fn.local )
      unzip( fn.local, exdir=file.path( p$data.directory, "polygons", "openstreetmap" )  )


      # above is a messy polygon .. tidy up using the coarse bounds from stats canada
      ns = data_spatial( "NS_boundary_statscan_crude", p=p)

      out = raster::shapefile( shapefilename_osm )
      out = spTransform( out, sp::CRS(p$aegis_proj4string_planar_km) )
      out = gSimplify( out, tol=1)  # in meters

      pg = gIntersection( out, ns, drop_lower_td=TRUE, byid=TRUE )

      pg = as( pg, "SpatialPolygonsDataFrame" )
      pg$id = 1
      pg = try( gUnaryUnion(pg, id=pg@data$id), silent=TRUE  )

      # sum(gIsValid(pg, byid=TRUE)==FALSE) # check if any bad polys?
      # pg = gBuffer(pg, byid=TRUE, width=0)
      # plot(pg)
      pg = sp::spChFIDs( pg, "NovaScotia" ) #fix id's

      save(pg, file=fn, compress=TRUE)
      return(pg)
    }

  # ---------------------------------


      if (DS=="NS_openstreetmaps_test") {
        # from open street maps data .. not fully working .. test only via osmdata
        fn = file.path( p$data.directory, "polygons", "NS_openstreetmaps.rdata" )
        if ( !redo ) {
          if ( file.exists(fn) )  {
            load( fn )
            if (exists("aegis_proj4string_planar_km", p)) pg = spTransform( pg, sp::CRS(p$aegis_proj4string_planar_km) )
            return (pg)
          }
        }

        install.packages("osmplotr")

        library("osmdata")
        library("osmplotr")
        library("maptools")

        library(sf)

        xlim=c(-66.5,-58)
        ylim=c(41,47.2)
        bb = c( xlim[1], ylim[1], xlim[2], ylim[2])


        o = getbb("Nova Scotia, Canada")

        Q <- opq(o)
        # Q <- add_osm_feature(Q, key='boundaries', value='administrative' )
        # Q <- add_osm_feature(Q, key='natural', value='lake' )
        Q <- add_osm_feature(Q, key = 'natural', value = 'water')

        Qxml <- osmdata_xml(Q)
        Qsp = osmdata_sp(Q, Qxml)

        bbox = get_bbox(o)
        map <- osm_basemap(bbox = bbox, bg = 'gray20')
        map <- add_osm_objects(map, y$osm_polygons, col = 'gray40')
        print_osm_map(map)


        # above is a messy polygon .. tidy up using the coarse bounds from stats canada
        ns = data_spatial( "NS_boundary_statscan_crude", p=p)

        out = raster::shapefile( shapefilename_osm )
        out = spTransform( out, sp::CRS(p$aegis_proj4string_planar_km) )
  #      out = gSimplify( out, tol=1)  # in meters

        pg = gIntersection( out, ns, drop_lower_td=TRUE, byid=TRUE )

        pg = as( pg, "SpatialPolygonsDataFrame" )
        pg$id = 1
        pg = try( gUnaryUnion(pg, id=pg@data$id), silent=TRUE  )

        # sum(gIsValid(pg, byid=TRUE)==FALSE) # check if any bad polys?
        # pg = gBuffer(pg, byid=TRUE, width=0)
        # plot(pg)
        pg = sp::spChFIDs( pg, "NovaScotia" ) #fix id's

        save(pg, file=fn, compress=TRUE)
        return(pg)
      }

  # --------------------------


      if (DS=="NS_ocean_openstreetmapdata") {
        # from open street maps data
        fn = file.path( p$data.directory, "polygons", "NS_ocean_openstreetmapdata.rdata" )

        if ( !redo ) {

        if ( file.exists(fn) )  {
          load( fn )
          if (exists("aegis_proj4string_planar_km", p)) pg = spTransform( pg, sp::CRS(p$aegis_proj4string_planar_km) )
          return (pg)
        }
        }

        message( "Downloading .. you will need to manully unzip this one." )

        fn1 = "http://data.openstreetmapdata.com/water-polygons-split-4326.zip"
        dir.local = file.path( p$data.directory, "polygons", "openstreetmap", "water-polygons-split-4326" )
        dir.create( dir.local, recursive=TRUE, showWarnings=FALSE )
        fn.local = file.path( dir.local, basename( fn1 ) )
        shapefilename_osm = file.path( dir.local, "water_polygons.shp" )

        download.file( url=fn1, destfile=fn.local )
        unzip( fn.local, exdir=file.path( p$data.directory, "polygons", "openstreetmap" )  )

        # above is a messy polygon .. tidy up using the coarse bounds from stats canada
        ns = data_spatial( "NS_boundary_statscan_crude", p=p)

        out = raster::shapefile( shapefilename_osm )
        out = spTransform( out, sp::CRS(p$aegis_proj4string_planar_km) )
  #      out = gSimplify( out, tol=1)  # in meters

        pg = gIntersection( out, ns, drop_lower_td=TRUE, byid=TRUE )

        pg = as( pg, "SpatialPolygonsDataFrame" )
        pg$id = 1
        pg = try( gUnaryUnion(pg, id=pg@data$id), silent=TRUE  )

        # sum(gIsValid(pg, byid=TRUE)==FALSE) # check if any bad polys?
        # pg = gBuffer(pg, byid=TRUE, width=0)
        # plot(pg)
        pg = sp::spChFIDs( pg, "NovaScotia" ) #fix id's

        save(pg, file=fn, compress=TRUE)
        return(pg)
      }




}
