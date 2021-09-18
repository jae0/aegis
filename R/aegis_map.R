

  aegis_map = function( xyz, depthcontours=TRUE, pts=NULL, colpts=FALSE,
      annot=NULL, annot.cex=2, leg = NULL,
      projection_map_proj4string = NULL,
      col.regions=FALSE, at=0:1,
      corners=NULL, rez=c(1,1), spatial_domain="SSE", display=FALSE, save=TRUE,
      pt.cex=0.5, pt.pch=16, pt.col='black',
      colorkey=NULL, fill=T, scalebar=NULL, landfillcolour="lightgrey", plotlines=NULL,
      ... ) {

    # map using levelplot ... no GMT dependency

		require( lattice )
    require( sf )
    

    pp = spatial_parameters( spatial_domain=spatial_domain )

    if (is.null(projection_map_proj4string)) {
      projection_map_proj4string = pp$aegis_proj4string_planar_km
    }

    xlim =ylim = NULL
    if(is.null(colorkey)) colorkey=list(space="right", labels=list(cex=3)) # these are lattice options

    if (ncol( xyz) == 2) { # assuming points
      xyz = cbind( xyz, 1)
      colorkey=FALSE
    }

    if ( is.null( xyz$plon) ) {
      names( xyz ) = c( "lon", "lat", "z" )
      xyz = lonlat2planar( xyz,   proj.type=projection_map_proj4string)
      xyz = xyz[ , c( "plon", "plat", "z" ) ]
      if ( !is.null(corners) ) {
        if ( is.null(corners$plon) ) corners = lonlat2planar( corners, proj.type=projection_map_proj4string )
      }

    } else {
      names( xyz ) = c( "plon", "plat", "z" )
    }

    if ( is.logical( colorkey) )  pts = xyz      # flag from above when only XY data are passed,.. after conversion to planar coords

    if ( !is.null(corners) ) {
      xlim = range( corners$plon)
      ylim = range( corners$plat)
    } else {
      xlim = range( xyz$plon )
      ylim = range( xyz$plat )
    }

    if ( ! is.null(pts) ) {
      if ( is.null( pts$plon) ) {
        pts = lonlat2planar(xyz,  proj.type=projection_map_proj4string )
        pts = pts[, c("plon", "plat")]
      }
    }

    if(fill){
      xyz$z[xyz$z>max(at)]=max(at)
      xyz$z[xyz$z<min(at)]=min(at)
    }


    lp = levelplot( z ~ plon+plat, data=xyz, aspect="iso", pts=pts, colpts=colpts,
      annot=annot, annot.cex=annot.cex, projection_map_proj4string=projection_map_proj4string,
      pp=pp,
      xlab="", ylab="", scales=list(draw=F), col.regions=col.regions, at=at, xlim=xlim, ylim=ylim,
      colorkey=colorkey, rez=rez, leg=leg, plotlines=plotlines,
      panel = function(x, y, z, rez=rez,  ...) {

        panel.levelplot (x, y, z, aspect="iso", rez=rez, ...)

        if ( !is.null(pts) ) {
          if (colpts) {  # overlay above with larger sized points, esp if there is colour
            colb = findInterval( z, at)
            for ( ii in 1:length(z) ) {
              panel.xyplot( pts$plon[ii], pts$plat[ii],  aspect="iso", col=col.regions[colb[ii]],
                  panel = panel.rect, height = rez[1], width = rez[2], cex=pt.cex, ... )
            }
          } else {
              panel.xyplot( pts$plon, pts$plat,  aspect="iso", col = pt.col, pch=pt.pch,
                  panel = panel.rect, height = rez[1], width = rez[2], cex=pt.cex, ... )
          }
        }
        if (depthcontours) {
 browser()
          isobs = aegis.bathymetry::isobath_db( p=p, depths=c( 100, 200, 300, 400, 500, 600, 700 ) )

          isobs = st_transform( isobs, crs=st_crs(projection_map_proj4string) )
          isobs = as( as( isobs, "Spatial"), "SpatialLines")
          
          depths1 = as.character( c(100, 300, 500, 700 ) )  
          depths2 = as.character( c(200, 400, 600 ) )

          for ( i in depths1 ) {
            browser()
            sp.lines( isobs[depths1,], col = rgb(0.2,0.2,0.2,0.5), cex=0.6 )
          }
          for ( i in depths2 ) {
            sp.lines( isobs[depths2,], col = rgb(0.3,0.3,0.3,0.5), cex=0.6 )
          }
        }

        if ( !is.null(plotlines) ) {
          lines.to.plot = aegis.polygons::area_lines.db( DS=plotlines )
          for ( pln in 1:length(lines.to.plot )) {
            ltp = lonlat2planar( lines.to.plot[[pln]], proj.type=projection_map_proj4string )
            panel.lines( ltp$plon, ltp$plat, col="black", lwd=1, lty=2 )
          }
        }

        #coastline
        coast = as( aegis.coastline::coastline_db( p=p, DS="eastcoast_gadm", project_to=st_crs(projection_map_proj4string) ) , "Spatial")

        sp.polygons( coast, col="black", cex=1, fill=landfillcolour)

        #legend
        lx = xlim[2]-xlim[1]
        ly = ylim[2]-ylim[1]
        if (is.null(leg) ) leg = c( xlim[2]-0.11*lx, ylim[1] + 0.11*ly )

        if (!is.null(scalebar)) {
          if (!is.numeric(scalebar)) {
             ruler = 100 # km
          } else {
             ruler = scalebar
          }
          panel.arrows( x0=leg[1]-ruler, y0=leg[2], x1=leg[1], y1=leg[2], angle=90, length=0.06, ends="both", lwd=3, col="black", ...)
          panel.text( x=leg[1]-ruler/2 , y=leg[2]+0.05*ly , paste(ruler,"km"), cex=annot.cex*0.75 )
        }

        if ( !is.null( annot ) ){
          panel.text( x=leg[1]-0.05*lx, y=leg[2]-0.05*ly, annot, cex=annot.cex )  # pos=2 is left of (right justified)
        }
    } # end panel
    ) # end levelplot

    return( lp )

  }
