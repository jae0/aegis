aegis_db_map = function( ip=NULL, p=NULL, type="all", voi=NULL ) {

  # ip is the first parameter passed in the parallel mode
  if (exists( "libs", p)) RLibrary( p$libs )

  if (is.null(voi)) if (exists("stmv_variables",p)) if(exists("Y", p$stmv_variables)) voi=p$stmv_variables$Y

  require( lattice )

  annot.cex = 1

  # -----------------------

  if ( type=="all" ) {

    allgrids = unique(c( p$spatial_domain_subareas, p$spatial_domain) )
    p$clusters =p$stmv_clusters[[1]]

    for ( gr in allgrids ) {
      print (gr)
      p1 = spatial_parameters(  p=p, spatial_domain=gr )
      aegis_db_map( p=p1, type="climatology" ) # no parallel option .. just a few
      parallel_run( aegis_db_map, p=p1, type="annual", voi=voi, runindex=list( yrs=p1$yrs)  )
    }

  }

  # -----------------------


  if ( type %in% c("annual" ) ) {
    projectdir = file.path(p$data_root, "maps", voi, p$spatial_domain, "annual" )
    dir.create( projectdir, recursive=T, showWarnings=F )

    if (is.null(ip)) ip = 1:length(p$yrs)

    # over-ride default dependent variable name if it exists

    loc = bathymetry.db(p=p, DS="baseline" )

    for (iy in ip ) {
      y = p$runs[iy, "yrs"]
      print(y)
      H = aegis_db( p=p, DS="predictions", year=y, ret="mean" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H)
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]
      datarange = aegis_lookup_mapparams( DS="datarange", voi ) # hardcoded data ranges
      if (is.null(datarange)) {
        datarange=quantile(xyz[,3], probs=c(0.05,0.95), na.rm=TRUE)
        datarange = seq( datarange[1], datarange[2], length.out=100 )
      }
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(voi), fixed=TRUE )
      outfn = paste( voi, "mean", y, sep=".")

      fn = file.path( projectdir, paste(outfn, "png", sep="." ) )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial_domain=p$spatial_domain )
      print(lp)
      dev.off()

      H = aegis_db( p=p, DS="predictions", year=y, ret="sd" )
      if (is.null(H)) next ()
      xyz = cbind(loc, H)
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]
      datarange = aegis_lookup_mapparams( DS="datarange", voi ) # hardcoded data ranges
      if (is.null(datarange)) {
        datarange=quantile(xyz[,3], probs=c(0.001,0.999), na.rm=TRUE)
        datarange = seq( datarange[1], datarange[2], length.out=100 )
      }
      if (length(unique(datarange)) == 1) next()
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(voi), fixed=TRUE )
      outfn = paste( voi, "sd", y, sep=".")

      fn = file.path( projectdir, paste(outfn, "png", sep="." ) )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial_domain=p$spatial_domain )
      print(lp)
      dev.off()

      print( file.path( projectdir, outfn))
    }

  }


  # ------------------------------


  if ( type %in% c("climatology" ) ) {
    projectdir = file.path(p$data_root, "maps", voi, p$spatial_domain, "climatology" )
    dir.create( projectdir, recursive=T, showWarnings=F )

    loc = bathymetry.db(p=p, DS="baseline" )

    H = aegis_db( p=p, DS="complete" )
    vnames = setdiff( names(H), c("plon", "plat" ))

    for (vn in vnames ) {
      xyz = cbind(loc, H[,vn])
      uu = which( is.finite(rowSums(xyz)))
      if (length(uu) < 10) next()
      xyz = xyz[uu,]

      datarange= NULL
      datarange = aegis_lookup_mapparams( DS="datarange", vn) # hardcoded data ranges
      if (is.null(datarange)) {
        datarange=quantile(xyz[,3], probs=c(0.001,0.999), na.rm=TRUE)
        datarange = seq( datarange[1], datarange[2], length.out=100 )
      }
      if (length(unique(datarange)) == 1) next()
      cols = color.code( "blue.black", datarange )
      annot = gsub( ".", " ", toupper(vn), fixed=TRUE )

      fn = file.path( projectdir, paste(vn, "png", sep="." ) )
      png( filename=fn, width=3072, height=2304, pointsize=40, res=300 )
        lp = aegis_map( xyz=xyz, depthcontours=TRUE, pts=NULL,
          annot=annot, annot.cex=annot.cex, at=datarange, col.regions=cols,
          corners=p$corners, spatial_domain=p$spatial_domain )
      print(lp)
      dev.off()

      print( file.path( projectdir, vn))
    }

  }

  return (NULL)
}
