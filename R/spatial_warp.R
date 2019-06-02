
spatial_warp = function( Z0, L0, L1, p0, p1, method="fast", L0i=NULL, L1i=NULL ) {
  #\\ regrid/reproject from p0 to p1 ; rgdal calls this "warping" ;)

  # ----------

  if (method=="fast") {

    L0_mat = matrix(NA, nrow=p0$nplons, ncol=p0$nplats )
    if (is.null(L0i)) L0i = array_map( "xy->2", L0[, c("plon", "plat")], gridparams=p0$gridparams )
    L0_mat[L0i] = Z0
    L0_grid = list(x=seq(min(p0$corners$plon), max(p0$corners$plon), by=p0$pres), 
                   y=seq(min(p0$corners$plat), max(p0$corners$plat), by=p0$pres), 
                   z=L0_mat)

    L1_interp = fields::interp.surface( L0_grid, loc=L1[, c("plon", "plat")] ) #linear interpolation
    ii = which( !is.finite( L1_interp ) )
    if ( length( ii) > 0 ) {
      L1_mat = matrix(NA, nrow=p1$nplons, ncol=p1$nplats )
      if (is.null(L1i)) L1i = array_map( "xy->2", L1[, c("plon_1", "plat_1")], gridparams=p1$gridparams )
      L1_mat[L1i] = L1_interp
      if (!exists("wght", p1)) p1$wght = fields::setup.image.smooth( nrow=p1$nplons,
        ncol=p1$nplats, dx=p1$pres, dy=p1$pres, theta=p1$pres, xwidth=4*p1$pres, ywidth=4*p1$pres )
      L1_sm = fields::image.smooth( L1_mat, dx=p1$pres, dy=p1$pres, wght=p1$wght )
      L1_grid = list(x=seq(min(p1$corners$plon), max(p1$corners$plon), by=p1$pres), 
               y=seq(min(p1$corners$plat), max(p1$corners$plat), by=p1$pres), 
               z=L0_mat)
      L1_interp[ii] = L1_grid$z[ii]
      jj = which( !is.finite( L1_interp ) )
      if ( length( jj) > 0 ) {
        L1_mat[L1i] = L1_interp
        L1_grid = list(x=seq(min(p1$corners$plon), max(p1$corners$plon), by=p1$pres), 
               y=seq(min(p1$corners$plat), max(p1$corners$plat), by=p1$pres), 
               z=L1_mat)
        L1_interp[jj] = fields::interp.surface( L1_grid, loc=L1[jj, c("plon_1", "plat_1")] ) #linear interpolation from smoothed surface
      }
    }
    return( L1_interp)

  }

}


