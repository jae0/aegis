  map_simple = function( toplot, plotarea, vn, resol=1, theta=resol*25, filterdistances=theta/2.3, annot=vn, spatial_domain="SSE", er=NULL ) {

    names( toplot ) = c("plon", "plat", "z")  # for aegis map, rename to z
    idup = which( duplicated( toplot[, c("plon", "plat")]  ))
    if (length(idup) > 0 ) toplot = toplot[-idup, ]

    idup = which( duplicated( plotarea[, c("plon", "plat")]  ))
    if (length(idup) > 0 ) plotarea = plotarea[-idup, ]


    distances =  rdist( plotarea[,c("plon", "plat")], toplot[ , c("plon", "plat") ] )
    distances[ which(distances < filterdistances ) ] = NA

    plocs = plotarea[ which( !is.finite( rowSums(distances) ) ), ]

    if (is.null(er)) er =  range( toplot[,"z"], na.rm=T) * c(0.95, 1.05)
    datarange = seq( er[1], er[2], length.out=50)

    u = fastTps( x=toplot[,c("plon","plat")], Y=toplot[,"z"], theta=theta )
    smp = cbind( plocs[,1:2], predict(u, xnew=plocs[,c("plon","plat")]))

    names( smp ) = c("plon", "plat", "z")  # for aegis map, rename to z

    cols = colorRampPalette(c("darkblue","cyan","green", "yellow", "orange","darkred", "black"), space = "Lab")

    lp = aegis_map( smp, xyz.coords="planar", depthcontours=TRUE, pts=toplot[,c("plon","plat")],
      annot=annot, annot.cex=2, at=datarange , col.regions=cols(length(datarange)+1),
      colpts=F, corners=p$corners, display=F, colorkey=NULL, plotlines="cfa.regions" )
    print(lp)

    return("Done")
  }

