
read_write_fast = function ( fn, data=NULL, compress="", filetype=NULL,
     compression_level=9,  ascii=FALSE, force_lower_case_extension=TRUE, ...)  {
     

    # required params for some functions below:
    version=NULL
    refhook=NULL

    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # note: fst also has read_fst/write_fst functions but they specialize on data table/frames and random access ... 
    # use RDS as container for compressed file
   
    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # copied from base::saveRDS readRDS and modified to allow other compression_levels and compression methods .. default uses fst's zstd and lz4
    # NOTE -- fst fails when file size exceeds R's vector limit .. needs to chunk .. so we default to qs

   
    FN = filenames(fn, force_lower_case_extension=force_lower_case_extension, clean=TRUE )  # decompose file name
    if (is.null(filetype)) {
        filetype = tolower(FN[["extension"]])
    } else {
        if (FN[["extension"]] != filetype) {
            FN[["extension"]] = filetype
            FN[["basename"]] = paste(FN[["corename"]], FN[["extension"]], sep=".")
            FN[["fullname"]] = file.path( FN[["dirname"]], FN[["basename"]] )
            message("Mismatch of filename extension and filetype ... \noverriding to filetype provided and changing file name: \n", FN[["fullname"]])
            message("If you do not want to change it, then override with correct filetype")
        }
    }

    fn = FN[["fullname"]]

    compression_level = as.integer( compression_level)

    if (is.null(data)) {  
        
        # no data to save so, must be a data load ... 
        
        if (is.character(fn)) {
            con = file(fn, raw=TRUE)
        } else {
            stop("bad 'fn' argument")
        }
        
        on.exit(close(con))

        o = NULL

        if ( filetype %in% c("rdata", "rda")) {
            o0 = ls()
            load(fn)
            o = ls()
            o = setdiff( o, c(o0, "o0") ) 
            o = setdiff( o, "o0" ) 
            if (length(o) > 0 ) {
                return( get(o) )
            } else {
                message("If you already have the rdata object's name in memory, this will fail to load, so set it to NULL first .")
                stop()
            }
        }

        if ( filetype == "rds") {
            o = try( readRDS(fn), silent=TRUE )  
        }
        if ( filetype == "qs") {
            o = try( qs::qread(fn ), silent=TRUE)
        }
        if ( filetype == "rdq") {
            o = try( qs::qread(fn ), silent=TRUE)
        }
        if ( filetype == "fst") {
            o = try(.Internal(unserialize(fst::decompress_fst(readBin(fn, "raw", file.size(fn))), refhook ) ))
        }

        if ( filetype %in% c("csv", "txt", "dat", "gz") ) {
            o = try( data.table::fread(fn), silent=TRUE )  
        }

        if (!inherits(o, "try-error"))  return( o )


        if ( ! filetype %in% c("rds", "qs", "rdq", "fst", "rda", "rdata" ) ) {

            o0 = ls()
            load(fn)
            o = ls()
            o = setdiff( o, c(o0, "o0") ) 
            o = setdiff( o, "o0" ) 
            if (length(o) > 0 ) {
                return( get(o) )
            } 

            o = try( readRDS(fn), silent=TRUE )  # input method for RDS
            if (!inherits(o, "try-error"))  return( o )
 
            o = try( qs::qread(fn ), silent=TRUE)
            if (!inherits(o, "try-error")) return( o )

            o = try(.Internal(unserialize(fst::decompress_fst(readBin(fn, "raw", file.size(fn))), refhook ) ), silent=TRUE )
            if (!inherits(o, "try-error")) return( o )

            o = try( data.table::fread(fn), silent=TRUE )  
            if (!inherits(o, "try-error")) return( o )

        }

        message( "Unrecognized file name extension ... \nForce by specifying filetype='...'" )
        stop()
    } 
    


    # data provided, so a save

    if (! inherits(fn, "character") )  stop( "fn needs to be a character")
    
    if (filetype %in% c("rda", "rdata", "rds") ) {
        con = NULL
        if (is.logical(compress)) { 
            if (compress) {
                con = gzfile(fn, "wb", compression = compression_level)
            } else {
                con = file(fn, "wb")
            }
        
        } else {
                
            if ( compress =="bzip2") con = bzfile(fn, "wb", compression = compression_level)
            if ( compress =="xz" )   con = xzfile(fn, "wb", compression = compression_level)
            if ( compress =="gzip" ) con = gzfile(fn, "wb", compression = compression_level)  # compress: 0-9
        }
        if (is.null(con)) con = gzfile(fn, "wb", compression = 1)
        on.exit(close(con))
        .Internal(serializeToConn(data, con, FALSE, version, refhook))

        return(fn)
    }

    if (filetype %in% c("fst") ) {

        compressor = NULL
        if ( compress =="fst-zstd" ) compressor = "ZSTD"
        if ( compress =="fst-lz4" )  compressor="LZ4"
        if (is.null(compressor)) compressor = "ZSTD"
        message("Large files > 2^31-1 bytes can cause issues ... use qs based methods")
        writeBin( 
            fst::compress_fst( 
                x=base::serialize(data, NULL, ascii, xdr=TRUE), 
                compressor=compressor, 
                compression=compression_level,   # note 0 - 100 (and not 0-9 as in RDS, etc)
                TRUE), 
            fn 
        ) 
        
        return(fn)
    }

    if (filetype %in% c("rdq", "qs") ) {
        preset = NULL
        if ( compress == "qs-custom" ) preset = "custom" # if custom, additional params are expected to be sent ...
        if ( is.null(preset)) {
            if ( compression_level %in% 9 ) preset = "archive"
            if ( compression_level %in% 7:8 ) preset = "high"
            if ( compression_level %in% 4:6 ) preset = "balanced"
            if ( compression_level %in% 1:3 ) preset = "fast"
            if ( compression_level %in% 0 )  preset = "uncompressed"
        }
        if (is.null(preset)) preset = "balanced"
 
        qs::qsave( x=data, file=fn, preset=preset, nthreads=8, ... )   # uses compress_level values from -50 to 22 for zstd and > 1 (higher is less compressed ) .. confusing so use presets
        return(fn)
    }


    if ( filetype =="rds" ) {
        saveRDS( object=data, file=fn, compress = TRUE ) 
    }


    if ( filetype == "direct_serial_connection" ) {
        con = file(fn, "wb" )
        on.exit(close(con))
        .Internal(serializeToConn(data, con, FALSE, version, refhook))  
        return(fn)
    }

    if (filetype %in% c("csv", "dat", "txt", "gz") ) {
        data.table::fwrite(data, file=fn, ...)   # data.table::fwrite have other options (including append, compress, etc) .. using fwrite directly is probably better
    }

    stop( "Error: filetype and/or compression not recognized" )


    if (0) {
        # test
        require(data.table)
        data(iris)

        read_write_fast( fn="~/tmp/iris_data.rds", data =iris )
        str( read_write_fast( fn="~/tmp/iris_data.rds" )  )

        iris2 = data.table(iris)
        save(iris2, file="~/tmp/iris2.rda")
        str( read_write_fast("~/tmp/iris2.rda") )

        read_write_fast( fn="~/tmp/iris_data.rdq", data =iris )
        str( read_write_fast( fn="~/tmp/iris_data.rdq" )  )
   
        read_write_fast( fn="~/tmp/iris data.RDQ", data =iris )
      
    }
   
}
