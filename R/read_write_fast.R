
read_write_fast = function ( file="", data=NULL, version=NULL, compress="zstd", compression_level=0, refhook=NULL, hash=TRUE)  {
    
    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # copied from base::saveRDS readRDS and modified to allow other compression_levels and compression methods .. default uses fst's zstd and lz4
    require(fst)

    if (is.null(data)) {  # no data to save so, must be to load ... 
        
        if (is.character(data)) message( "First argument is file ('file') name and second is a data object ('data'), if not then use named arguments" )
        
        if (is.character(file)) {
            con <- gzfile(file, "rb")
            on.exit(close(con))
        } else if (inherits(file, "connection")) {
            if (inherits(file, "url")) { 
                con = gzcon(file)
            } else {
                con = file
            }
        } else {
            stop("bad 'file' argument")
        }
        o = try( .Internal(unserializeFromConn(con, refhook)), silent=TRUE )  # see if it is an RDS file

        if (inherits(o, "try-error")) {
            return( .Internal(unserialize(fst::decompress_fst(readBin(file, "raw", file.size(file))), refhook ) ) )
        } else {
            return( o )
        }

    } else {   # save data 
    
        con = NULL
        if (inherits(file, "connection")) {
            if (inherits(file, "url")) {
                con= gzcon(file)
            } else {
                con = file
            }
        } else {
            if (is.logical(compress)) { 
                if (compress) {
                    con = gzfile(file, "wb", compression = compression_level)
                } else {
                    con = file(file, "wb")
                }
            } else {
                if ( compress =="bzip2" ) con = bzfile(file, "wb", compression = compression_level)
                if ( compress =="xz" ) con = xzfile(file, "wb", compression = compression_level)
                if ( compress =="gzip" ) con = gzfile(file, "wb", compression = compression_level)  # compress: 0-9
                if ( compress =="zstd" ) {
                    writeBin( fst:::fstcomp( serialize(data, NULL), "ZSTD", as.integer(compression_level), hash), file ) 
                    return(file)
                }

                if ( compress =="lz4" ){
                    writeBin( fst:::fstcomp( serialize(data, NULL), "LZ4", as.integer(compression_level), hash), file ) 
                    return(file)
                }
            }
        }
        if (is.null(con)) con = file(file, "wb" )
        on.exit(close(con))
        .Internal(serializeToConn(data, con, FALSE, version, refhook))
    }        
   
}
