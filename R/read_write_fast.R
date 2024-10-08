
read_write_fast = function ( file, data=NULL, version=NULL, compress="qs-preset", qs_preset="high", 
     compression_level=1, refhook=NULL, hash=TRUE, ascii=FALSE, xdr=TRUE, ...)  {
     

    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # note: fst also has read_fst/write_fst functions but they specialize on data table/frames and random access ... 
    # use RDS as container for compressed file
   
    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # copied from base::saveRDS readRDS and modified to allow other compression_levels and compression methods .. default uses fst's zstd and lz4
    # NOTE -- fst fails when file size exceeds R's vector limit .. needs to chunk .. so we default to qs
    
    compression_level = as.integer( compression_level)
    


    if (is.null(data)) {  # no data to save so, must be to load ... 
        
        if (is.character(data)) message( "First argument is file ('file') name and second is a data object ('data'), if not then use named arguments" )
        
        if (is.character(file)) {
            con = file(file, raw=TRUE)
        } else if (inherits(file, "connection")) {
            con = file
        } else {
            stop("bad 'file' argument")
        }
        
        on.exit(close(con))

        o = try( readRDS(file), silent=TRUE )  # input method for RDS
        
        if (!inherits(o, "try-error"))  return( o )
 
        o = try( qs::qread(file ), silent=TRUE)
        if (!inherits(o, "try-error")) return( o )

        o = try(.Internal(unserialize(fst::decompress_fst(readBin(file, "raw", file.size(file))), refhook ) ))

        # an fst-object
        return( o )
 
    } else {   # save data 
    
        con = NULL
        if (inherits(file, "connection")) {
            con = file
        } else {
            if (is.logical(compress)) { 
                if (compress) {
                    con = gzfile(file, "wb", compression = compression_level)
                    on.exit(close(con))
                    .Internal(serializeToConn(data, con, FALSE, version, refhook))
                    return(file)
                } else {
                    con = file(file, "wb")
                    on.exit(close(con))
                    .Internal(serializeToConn(data, con, FALSE, version, refhook))
                    return(file)
                }
            } else {
                if ( compress =="bzip2" ) {
                    con = bzfile(file, "wb", compression = compression_level)
                    on.exit(close(con))
                    .Internal(serializeToConn(data, con, FALSE, version, refhook))
                    return(file)
                }
                
                if ( compress =="xz" ) {
                    con = xzfile(file, "wb", compression = compression_level)
                    on.exit(close(con))
                    .Internal(serializeToConn(data, con, FALSE, version, refhook))
                    return(file)
                }
                
                if ( compress =="gzip" ) {
                    con = gzfile(file, "wb", compression = compression_level)  # compress: 0-9
                    on.exit(close(con))
                    .Internal(serializeToConn(data, con, FALSE, version, refhook))
                    return(file)
                }

                if ( compress =="fst-zstd" ) {
                    writeBin( fst::compress_fst( x=base::serialize(data, NULL, ascii, xdr=xdr), compressor="ZSTD", compression=compression_level, hash), file ) 
                    return(file)
                }
                if ( compress =="fst-lz4" ) {
                    message("Large files > 2^31-1 bytes causes issues ... use qs based methods")
                    writeBin( fst::compress_fst( x=base::serialize(data, NULL, ascii, xdr=xdr), compressor="LZ4", compression=compression_level, hash), file ) 
                    return(file)
                }
                if ( compress =="qs-preset" ) {
                    qs::qsave( x=data, file=file, preset=qs_preset ) # "high, balanced, fast, archive, uncompressed, custom"
                    return(file)
                }
                if ( compress =="qs-custom" ) {
                    qs::qsave( x=data, file=file, preset="custom", ... ) 
                    return(file)
                }

                if ( compress =="RDS" ) {
                    saveRDS( object=data, file=file, compress = TRUE ) 
                    return(file)
                }

                # save as RDS as last resort
                saveRDS( object=data, file=file, compress = FALSE ) 
                return(file)

            }
        }

        if (is.null(con)) con = file(file, "wb" )
        
        on.exit(close(con))

        .Internal(serializeToConn(data, con, FALSE, version, refhook))
  
        return( message( "save completed", file )   )

    }        
   
}
