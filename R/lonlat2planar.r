
  lonlat2planar = function ( x, proj.type, input_names=c("lon", "lat"), newnames = c("plon", "plat") ) {
    #\\ convert lon/lat to a projected surface using proj
    #\\ proj.type can be an internal code such as "utm20" or a proj4 argument
    #\\ output scale is defined in the +units=km (default for bio) or +units=m (default for proj)
    # first try an internal conversion /lookup for CRS
  
    datatype = ifelse("data.table" %in% class(x), "data.table", "data.frame")

    require(data.table)
    setDT(x)

    pjj = NULL
    pjj = projection_proj4string(proj.type)
    if (!is.null(pjj)) {
      proj4.params = try( sf::st_crs( pjj ), silent=TRUE )
    } else {
      proj4.params = try( sf::st_crs( proj.type ), silent=TRUE )
    }

    if ( "try-error" %in% class( proj4.params) ) {
      if (!is.null(proj.type)) proj4.params = try( sf::st_crs( as.character(proj.type) ), silent=TRUE )
    }
    if ( "try-error" %in% class( proj4.params) ) {
      print( proj.type )
      warning( "Projection not recognised")
    }

    crsX = proj4.params$input
    if ( ! grepl("units", crsX) ) {
      print (crsX)
      stop( "The proj4 CRS requires an explicit +units=km ")
    }

    # y = rgdal::project( as.matrix(x[,input_names]), proj=crsX , inv=F )
    y = sf::sf_project( from=sf::st_crs("EPSG:4326"), to=proj4.params, pts=as.matrix(x[,input_names, with=FALSE]))

    colnames(y) = newnames
    for (i in 1:length( newnames)) {
      if ( newnames[i] %in% colnames(x) ) x[[newnames[i]]] = NULL
    }
    x = cbind(x,y)
    if (datatype != "data.table") setDF(x)
    return (x)
  }
