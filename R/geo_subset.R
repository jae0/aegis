
geo_subset = function( spatial.domain, Z ) {
  #\\ filter locations based upon depths and location of spatial.domain
  #\\ most are in planar coords (so require plon, plat and z {depth} )

  # trim to extents
  ps = spatial_parameters( spatial.domain=spatial.domain ) # obtain internal projection params
  inside = which(
    Z$plon >= ps$corners$plon[1] & Z$plon <= ps$corners$plon[2] &
    Z$plat >= ps$corners$plat[1] & Z$plat <= ps$corners$plat[2]  )
  if (length( inside ) > 0) Z = Z[ inside, ]


  if ( spatial.domain == "canada.east.superhighres" ) {
    Z = Z[ which( Z$z < 5000 & Z$z > 0) , ]
  }


  if ( spatial.domain == "canada.east.highres" ) {
    Z = Z[ which( Z$z < 5000 & Z$z > 0) , ]
  }

  if ( spatial.domain == "canada.east" ) {
    Z = Z[ which(Z$z < 5000 & Z$z > 0 ) ,]
  }

  if ( spatial.domain == "SSE.mpa" ) {
    Z = Z[ which(Z$z < 2000 & Z$z > 0 ) ,]
    bnd = polygon.db( id="scotia.fundy.with.buffer" )
    bnd = lonlat2planar( bnd, proj.type=ps$internal.crs ) # convert to internal projection
    jj = which( sp::point.in.polygon( Z$plon, Z$plat, bnd$plon, bnd$plat) != 0 )
    if (length( jj) > 0) Z = Z[ jj, ]
  }

  if ( spatial.domain =="SSE" ) {
    Z = Z[ which(Z$z < 800 & Z$z > 0 ) ,]
  }

  if ( spatial.domain == "snowcrab" ) {
    #\\ NOTE::: snowcrab baseline == SSE baseline, except it is a subset

    kk = which( Z$z < 350 & Z$z > 10  )
    if (length( kk) > 0) Z = Z[ kk, ]

    jj = polygon_inside( Z[,c(1:2)], region="cfaall", planar=T, proj.type=ps$internal.crs )
    if (length( jj) > 0) Z = Z[ jj, ]

    # filter out area 4X
    corners = data.frame( cbind(
      lon = c(-63, -65.5, -56.8, -66.3 ),
      lat = c( 44.75, 43.8, 47.5, 42.8 )
    ) )
    corners = lonlat2planar( corners, proj.type=ps$internal.crs )
    dd1 = which( Z$plon < corners$plon[1] & Z$plat > corners$plat[1]  )
    if (length( dd1) > 0) Z = Z[- dd1, ]
    dd2 = which( Z$plon < corners$plon[2] & Z$plat > corners$plat[2]  )
    if (length( dd2) > 0) Z = Z[- dd2, ]
    dd3 = which( Z$plon > corners$plon[3] ) # east lim
    if (length( dd3) > 0) Z = Z[- dd3, ]
    dd4 = which( Z$plon < corners$plon[4] )  #west lim
    if (length( dd4) > 0) Z = Z[- dd4, ]
    dd5 = which( Z$plat > corners$plat[3]  ) # north lim
    if (length( dd5) > 0) Z = Z[- dd5, ]
    dd6 = which( Z$plat < corners$plat[4]  )  #south lim
    if (length( dd6) > 0) Z = Z[- dd6, ]
  }
  return(Z)
}

