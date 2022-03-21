spatial_neighbourhood_plot = function( poly, crs_view=NULL, additional_features=NULL, method="webbrowser" ) {

  if (is.null( crs_view )) {
    if (exists("crs_planar_default", globalenv() )) {
      crs_view = globalenv()[["crs_planar_default"]]
    } else {
      crs_view = st_crs("+proj=utm +zone=20 +ellps=WGS84 +units=m")
    }
  }

  if (is.character(poly)) {
    NB_graph = spatial_neighbourhood( poly )
    poly = polygon_data( geography=poly, subtype="neighbourhoods" )  # overwrite!
  } else {
    NB_graph = attr( poly, "NB_graph" )
  }

  poly = st_transform(poly, crs=crs_view )

  xy = st_geometry(st_centroid(poly))

  if (method=="basic") {
    plot(st_geometry(poly), col="lightgray", lwd=0.8, lty="dashed", reset=FALSE)
    plot(NB_graph, coords=xy, add=TRUE, col="#94350f", lwd=2 )
    print( summary(NB_graph) )
  }

  if (method=="simple") {
    nb = listw2lines( nb2listw(NB_graph), coords= st_coordinates(xy), as_sf=TRUE )
    plot(poly["AUID"], reset=FALSE)
    plot( nb[3], add=TRUE )
  }

  if (method=="webbrowser") {
    nb = listw2lines( nb2listw(NB_graph), coords= st_coordinates(xy), as_sf=TRUE )
    st_crs(nb) = crs_view

    require(tmap)

    additional_features = additional_features +
      tm_shape( nb, projection=crs_view ) +
        tm_lines( col="gray", alpha=0.8, lwd=2.5 )

    tmout =  map_polygons_webbrowser(
        data = poly,
        vn = "AUID",
        vn_title = paste( "Neighbourhood graph\n", attributes(poly)$geography ),
        additional_features=additional_features
    )
    print(tmout)

  }

  if (method=="edit_neighbourhoods") {
    dev.new()
    edit.nb(NB_graph, coords=st_coordinates(xy), polys=as(poly, "Spatial"), use_region.id=TRUE)
  }

}


