
spatialpolygon_to_raster = function( sp_poly, res=100, vn="dummy_value", return.counts.in.spatial.polygons=FALSE ) {

    require(raster)
    require(fasterize)

    sp_poly$poly_id = 1:length(sp_poly)

    if (vn=="dummy_value") sp_poly[[vn]] = 1 # dummy value

    vni = which( names(sp_poly) == vn )

    raster_template = raster(extent(sp_poly)) # +1 to increase the area
    res(raster_template) = res  # in meters
    crs(raster_template) = projection(sp_poly) # transfer the coordinate system to the raster

    S = fasterize( as(sp_poly, "sf"), raster_template, field="poly_id" )
    poly_id = values(S)

    ncells = as.data.frame.table( xtabs(~poly_id, na.action=na.omit), stringsAsFactors=FALSE )
    names(ncells) =c("poly_id", "ncells")
    # ncells$poly_id = as.character(ncells$poly_id)
    sp_poly = merge( sp_poly, ncells, by="poly_id", all.x=TRUE, all.y=FALSE, sort=FALSE)

    # poly_area = sapply(slot(sp_poly, "polygons"), slot, "area")
    vn_new = paste( vn, "_per_ncells", sep="" )
    dta = slot(sp_poly, "data")

    ng = length(which(is.finite(dta[,vn])))
    nc = length(which(is.finite(dta[,"ncells"])))

    if (nc != ng ) {
        warning( "Discretization resulted in some polygons without enough information" )
    }

    sp_poly[[vn_new]] = dta[,vn] / dta[,"ncells"]

    if (return.counts.in.spatial.polygons) return( sp_poly )

    values(S) = slot(sp_poly, "data")[poly_id, vn_new]

    return(S)

      if (0) {
        # only examples of working with polygons:
        b = spChFIDs(b, paste("b", row.names(b), sep="."))
        library(rgeos)
        p=SpatialPoints(list(lng,lat), proj4string=crswgs84)
        gContains(fdg,pt)

        p=list()
        p$root.directory = file.path( "/home", "nath") # Jae -- linux
        p$project.directory = file.path( p$root.directory, "NSspatial")
        p$data.directory    = file.path( p$project.directory, "data" )
        p$mypalette = brewer.pal(6, "YlOrRd")
        p$aegis_proj4string_planar_km = CRS( "+proj=omerc +lat_0=44.6475 +lonc=-63.580278 +gamma=0.0 +k=1 +alpha=332 +x_0=0 +y_0=0 +ellps=WGS84 +units=km" )

        if (exists("aegis_proj4string_planar_km", p)) spdata = spTransform( spdata, sp::CRS(p$aegis_proj4string_planar_km) )
        uid = "PRCDDA"
        spdata$uid = spdata[[uid]]
        spdata$Total = spdata$POP2001


        # example of selection / fitering mechanisms that work well
        spdata_coords = coordinates( spdata)
        spdata = cbind( spdata, spdata_coords)
        # find Sable Island
        # spplot(spdata, uid, scales=list(draw = TRUE) )
        si=subset( spdata, subset=coordinates(spdata)[,2] < -1e5 )$uid  # sable island 12090845
        # remove Sable
        if (length(si)>0) spdata = subset( spdata, subset={uid != si})
        # row.names(spdata) = spdata[["uid"]]

        # coast line with higher detail from world coastline database, download and convert
        # province = data_spatial( "NSCoastlineWorldmap", p=p )
        province = data_spatial( "NS_coastline_statscan", p=p )
        province = gSimplify(province, tol = 1) # simplify the polgons a tad (tweak 0.00001 to your liking)

        spdata_poly = gSimplify(spdata, tol = 1) # simplify the polgons a tad (tweak 0.00001 to your liking)
        spdata_poly = rgeos::gIntersection( province, spdata, drop_lower_td=TRUE, byid=TRUE ) # crop
        # sum(gIsValid(spdata_poly, byid=TRUE)==FALSE) # check if any bad polys?
        # spdata_poly = gBuffer(spdata_poly, byid=TRUE, width=0)
        # plot(spdata_poly)
        spdata_poly = sp::spChFIDs( spdata_poly, gsub( "^12[[:space:]]1[[:space:]]", "", names(spdata_poly) ) ) #fix id's
        spdata_poly = spdata_poly[ order(row.names(spdata_poly)) ]
        spdata_df = data.frame( uid=names(spdata_poly), stringsAsFactors=FALSE )
        spdata_df = merge( spdata_df, as.data.frame(spdata), by="uid", all.x=TRUE, all.y=FALSE )
        spdata_df = spdata_df[ order(spdata_df[["uid"]]) , ]
        row.names(spdata_df) = spdata_df[["uid"]]
        spdata = SpatialPolygonsDataFrame( spdata_poly, spdata_df )
        spdata = sp::spChFIDs( spdata, row.names(spdata_poly) )  #fix id's
        spdata$SA = sapply(slot(spdata , "polygons"), slot, "area")

      }


}
