
  array_map = function( method, coords, dims=NULL, origin=NULL, res=NULL, gridparams=NULL ) {
    #// array indexing from 1d to nd and nd to 1d
    # .. for higher dimensions, just follow the patterns
    # coords are input coords
    # dims are dimension sizes
    # origin min coord values
    # res resolution (dx, dy)

    coords = as.matrix(coords)

    if ( !is.null(gridparams) ) {
      dims = gridparams$dims
      origin = gridparams$origin
      res = gridparams$res
    }

    if (method=="ts->2") {
      # time, season index, identical to xy->2
      return( aegis_floor( cbind(
        (coords[,1]-origin[1])/res[1], # year index
        (coords[,2]-origin[2])/res[2]  # season index
        # as.numeric( cut( B$dyear, breaks=dyears_cuts , include.lowest=T, ordered_result=TRUE ) ), #season
      )) + 1L )
    }

    if (method=="ts->1") {
      # time, season index, same as xy->1, except order
      ij = aegis_floor( cbind(
        (coords[,1]-origin[1])/res[1], # year index
        (coords[,2]-origin[2])/res[2]  # season index
        # as.numeric( cut( B$dyear, breaks=dyears_cuts , include.lowest=T, ordered_result=TRUE ) ), #season
      ))
      return( c( ij[,2] + ij[,1]*dims[2] +1L) ) # same as 2->1, c makes it a vector
    }


    if (method=="xy->2") {
      return( aegis_floor( cbind( (coords[,1]-origin[1])/res[1] , (coords[,2]-origin[2])/res[2]) ) +1L  ) # do NOT use aegis_floor FP issues cause error
    }

    if (method=="xy->1") {
      ij = aegis_floor( cbind( (coords[,1]-origin[1])/res[1], (coords[,2]-origin[2])/res[2] ) )  # same as "xy->2"
      return( c( ij[,1] + ij[,2]*dims[1] +1L) ) # same as 2->1; c makes it a vector
    }

    if (method=="1->xy") {
      j = coords-1 # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j
      x = x * res[1] + origin[1]
      y = y * res[2] + origin[2]
      return( cbind(x,y) )  # +1 returns to R-indexing
    }


    if (method=="2->xy") {
      # -1 to go to c-indexing
      coords = coords - 1L
      x = coords[,1]  * res[1] + origin[1]
      y = coords[,2]  * res[2] + origin[2]
      return( cbind(x,y) ) # same as 2->1
    }


    if (method=="2->1") {
      coords = coords -1L
      return( c( coords[,1] + coords[,2]*dims[1] + 1L) )  #+1 to get r-indexing; c makes it a vector
    }

    if (method=="3->1") {
      coords = coords -1L
      return( c( coords[,1] + coords[,2]*dims[1] + coords[,3]*dims[1]*dims[2] +1L ) ) # c makes it a vector
    }

    if (method=="4->1") {
      coords = coords -1L
      return( c( coords[,1] + coords[,2]*dims[1] + coords[,3]*dims[1]*dims[2] + coords[,4]*dims[1]*dims[2]*dims[3] + 1L) ) # c makes it a vector
    }


    if (method=="3->2") {
      ii = array_map( "3->1" , coords, dims )
      jj = array_map( "1->2" , ii, dims )
      return( jj )
    }

    if ( method=="1->2" ) {
      j = coords - 1L # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j
      return( cbind(x,y)+1L )  # +1 returns to R-indexing
    }

    if ( method=="1->3" ) {
      j = coords - 1L # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j %%  dims[2]
      j = j %/% dims[2]
      z = j
      return( cbind(x,y,z) + 1L ) # +1 returns to R-indexing
    }

    if ( method=="1->4" ) {
      j = coords - 1L # -1 converts to C-indexing
      x = j %%  dims[1]
      j = j %/% dims[1]
      y = j %%  dims[2]
      j = j %/% dims[2]
      z = j %%  dims[3]
      j = j %/% dims[3]
      a = j
      return( cbind(x,y,z,a) + 1L ) # +1 returns to R-indexing
    }
  }

