
map_area_unit_problem = function( inp = NULL, logz=TRUE, just_return_results=FALSE ) {

  require(aegis)
  require(aegis.bathymetry)

  if (is.null(inp)) {
    p = list()
    p$data_root = project.datadirectory( "aegis", "bathymetry" )
    p$datadir = file.path( p$data_root, "data" )
    fn = file.path(p$datadir, "maup.rdata")
    fn_res = file.path(p$datadir, "maup_summary.rdata")
    if (just_return_results) {
      load(fn_res)
      return(u)
    }
    if (file.exists(fn) ) load( fn )
    if (is.null(inp)) {
      inp = bathymetry_db( p=p, "z.lonlat.rawdata")
      box = list( lon=c(-55,-45), lat=c(40, 50) )
      inp = inp[ which(
        inp$lon > box$lon[1] & inp$lon < box$lon[2] &
        inp$lat > box$lat[1] & inp$lat < box$lat[2]
      ), ]
      inp = lonlat2planar( inp, proj.type=projection_proj4string("utm20"))
      save (inp, file=file.path(p$datadir, "maup.rdata"), compress=TRUE )
    }
  }

  xvalues = inp[,"plon"]
  yvalues = inp[,"plat"]
  zvalues = inp[,3]

  xrange = range( xvalues, na.rm=TRUE )
  yrange = range( yvalues, na.rm=TRUE )
  dmin = min( diff(xrange),  diff(yrange) )

  if (logz) zvalues = log(zvalues+6000)

  results=list()
  results$global = list(
    mean = mean( zvalues, na.rm=TRUE),
    sd = sd(zvalues, na.rm=TRUE),
    n = length(zvalues),
    min = min(zvalues, na.rm=TRUE),
    max = max(zvalues, na.rm=TRUE),
    median = median(zvalues, na.rm=TRUE),
    resolution = sqrt( min( diff( sort(unique(xvalues))))^2 + min(diff(sort(unique(yvalues)) ))^2)
  )

  for (discretized_n in c(2, 3, 5, 7, 10, 15, 20, 40, 60, 80, 100, 200, 400, 800, 1000, 2000, 5000) ) {

    minresolution = rep( dmin / discretized_n, 2)

    # basic aggregation in 2D
    res = tapply(
      X=zvalues,
      INDEX=list(
        floor( xvalues / minresolution[1] + 1) * minresolution[1],
        floor( yvalues / minresolution[2] + 1) * minresolution[2]),
        FUN = function(w) {mean(w, na.rm=TRUE)},
        simplify=TRUE
    )
    res = as.data.frame( as.table (res) )
    res[,1] = as.numeric(as.character( res[,1] ))
    res[,2] = as.numeric(as.character( res[,2] ))
    res = res[ which( is.finite( res[,3] )) ,]
    names(res) =c("x", "y", "z")

    results[[as.character(discretized_n)]] = list(
      mean = mean( res[,3], na.rm=TRUE),
      sd = sd(res[,3], na.rm=TRUE),
      n = length(res[,3]),
      min = min(res[,3], na.rm=TRUE),
      max = max(res[,3], na.rm=TRUE),
      median = median(res[,3], na.rm=TRUE),
      resolution = diff(xrange)/discretized_n
    )

  }

  u = do.call(rbind.data.frame, results)  # list to data frame
  u = u[ order(u$resolution), ]

  save(u, file=fn_res, compress=TRUE)

  if (0) {
    x = log(u$resolution )
    x = log(  u$n / u$resolution^2  )  # data density
    yrange = range( u$min, u$max )
    plot(mean ~ x, u, pch=20, ylim=yrange)
    points(median ~ x, u, col="green", pch=20)
    lines(min ~ x, u, col="red", lwd=4)
    lines(max ~ x, u, col="blue", lwd=4)
    lines( I(mean+sd) ~ x, u, col="gray", lwd=2)
    lines( I(mean-sd) ~ x, u, col="gray", lwd=2)

    plot( sd~ x, u)
  }

  return(u)
}
