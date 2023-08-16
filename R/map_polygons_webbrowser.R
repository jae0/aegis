
map_polygons_webbrowser = function(
    data,
    vn = "pop_total",
    vn_filename = NULL,
    map_projection = st_crs(data),
    additional_features=NULL,
    ...
 ) {

   # simple maps using tmap and common set of options
    require(sf)
    require(tmap)

    ellps = list(...)

    data = st_transform(data, map_projection)
    # coastline = st_transform(coastline, map_projection)

    tmap_mode("view")

    plt = tm_basemap(leaflet::providers$CartoDB.Positron, alpha=0.8)

    plt = plt +
      tm_shape( data  ) +
        tm_polygons(
            col  = vn,
            # style = ifelse ( exists("style", ellps), ellps[["style"]], "fixed" ),
            # breaks = ifelse ( exists("vn_breaks", ellps), ellps[["vn_breaks"]], quantile( ns[[vn]], probs=seq(005, 0.975, by=0.1), na.rm=TRUE) ),
            title = ifelse ( exists("vn_title", ellps), ellps[["vn_title"]], vn ),
            border.col = "lightgray",
            colorNA = NULL,
            showNA=FALSE,
            lwd = 0.75,
            palette = ifelse ( exists("palette", ellps), ellps[["palette"]], "-viridis"),
            border.alpha = 0.75,
            alpha=0.95,
            legend.is.portrait = FALSE )


    if (!is.null(additional_features) ) {
      # e.g. management lines, etc
      plt = plt + additional_features
    }

    plt = plt +
      tm_scale_bar( position=c("right", "bottom" ), width=0.2, text.size=0.7) +
      tm_layout(legend.outside = TRUE, legend.outside.position="bottom", legend.text.size= 0.77, frame=FALSE )

    return(plt)



    if (!is.null(vn_filename)) {
      savePlot( filename=vn_filename, type='png' )
      return( vn_filename )
    }


    if (0) {
      # example call:
      map_polygons_webbrowser(
        data = ns,
        vn = "pop_total",
        vn_title = "Census 2016 DA population count",
        vn_breaks = quantile( ns[,vn], probs=seq(0, 1, by=0.1), na.rm=TRUE),
        vn_filename = file.path( work.directory, "census_population_counts.png")
      )

    }

}