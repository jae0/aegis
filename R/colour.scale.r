colour.scale = function( type="seis", nlevels=10, x=NULL, transparency=0.9 ) {
    # if (length( type) > 1 ) {
    #   colours = colorRampPalette( type, space = "rgb") (nlevels)
    # } else {
      colours = color.code( type, n=nlevels-1 )
    # }

    cols = data.frame( code=as.character(colours) ) 

    cols$code = sapply( cols$code, rbg2bgr )
    transp.hex = as.hexmode( round(transparency * 255 ) )
    cols$code = gsub( "#", transp.hex, cols$code )   
    er = range ( x, na.rm=T )
    upperbounds = seq( er[1], er[2], length.out=nlevels+1 ) [2:(nlevels+1)]
    y = findInterval( x, upperbounds ) + 1
    y[ which( y > nlevels) ] = nlevels
    return( list(y=y, cols=cols ) )
  }


