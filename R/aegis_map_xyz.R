
aegis_map_xyz = function( x, y, z, ncolors=100, color.code.type="seis", color.sequence=NULL, cex=1, xylines=list(), xypoints=list(), ... ) {

  require(lattice)
  zrange = range(z, na.rm=TRUE)
  breaks = seq( zrange[1], zrange[2], length.out=ncolors)
  if ( color.code.type =="manual" ) {
    if ( is.null( color.sequence ) ) stop( "color.sequence must be specified if using manual method " )
    cols = colorRampPalette(c("red", "white","blue"), space = "Lab") (breaks)
  } else {
    cols = color.code( color.code.type, breaks )  ## see color.code.r to contruct your own
  }
  zcol = cols[ cut( z, ncolors, label = FALSE) ]
  data = data.frame(x=x )
  data$y=y
  data$z=z
  data$zcol = zcol
  data = data[ rev(order( z )) , ]  # sort by color code
  pl = levelplot( z~ y + x, cex=cex, aspect="iso", data=data, xylines=xylines, xypoints=xypoints, ...,
    panel=function( x, y, subscripts, data=data, ... ) {
      panel.xyplot( x, y, col=zcol, fill = zcol, pch=21, cex=cex )
      nl = length( xylines)
      if (nl>0) {
        for (i in 1:nl) panel.lines ( xylines[[i]][,1] , xylines[[i]][,2] )
      }
      np = length( xypoints )
      if (np>0) {
        for (i in 1:np) panel.xyplot ( xypoints[[i]][,1] , xypoints[[i]][,2], pch=21, cex=cex/100 )
      }
    } )

  print(pl)
  return(pl)

  if (0) {
  # example usage
    project.library( "aegis")
    require(sp)
    require(lattice)
    data(meuse)
    z = meuse$lead
    ncolors = 100 ## number of colours
    drange = range(z, na.rm=TRUE)
    cols = color.code( "seis", seq( drange[1], drange[2], length.out=ncolors) ) [ cut( z, ncolors, label = FALSE) ]  ## see color.code.r to contruct your own
    zi <- rev(order( z ))
    xyplot( y ~ x, data=meuse[ zi,] , aspect="iso", col=cols[zi], fill = cols[zi], pch=21, cex=1 )
    o = aegis_map_xyz( x=meuse$x, y=meuse$y, z=meuse$lead, xylines=list( meuse[1:5, c("x", "y")] ), xypoints = list(meuse[6:8, c("x", "y")] ) )
  }
}


