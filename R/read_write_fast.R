read_write_fast = function ( fn, data=NULL, compress="", file=NULL, filetype=NULL, 
     compression_level=7,  ascii=FALSE, force_lower_case_extension=TRUE, override_extension=FALSE, ...)  {
     
    # required params for some functions below:
    version=NULL
    refhook=NULL

    if (!is.null(file)) fn = file
    
    # decompose file name
    FN = filenames(fn )  #, force_lower_case_extension=force_lower_case_extension, clean=TRUE )  

    if (is.null(filetype)) filetype = FN[["extension"]]
    filetype = tolower(filetype)

    if (FN[["extension"]] != filetype ) {
        
        if (tolower(FN[["extension"]]) == filetype) {
            message("Mismatch of filename extension and filetype letter case ... consider using only lower case")
        }

        if (override_extension) {
            FN[["extension"]] = filetype
            FN[["basename"]] = paste(FN[["corename"]], FN[["extension"]], sep=".")
            FN[["fullname"]] = file.path( FN[["dirname"]], FN[["basename"]] )
            message("Overriding to filetype provided and changing file name: \n", FN[["fullname"]])
            message("If you do not want to change it, then override with correct filetype")
        }
    }

    fn = FN[["fullname"]] 

    compression_level = as.integer( compression_level)

    if (is.null(data)) {  
        
        # no data to save so, must be a data load ... 
        if (! file.exists(fn)) {
            stop( "file not found: ", fn )
        }

        if (is.character(fn)) {
            con = file(fn, raw=TRUE)
        } else {
            stop("bad 'fn' argument")
        }
        
        on.exit(close(con))

        o = NULL

        if ( filetype %in% c("rdata", "rda")) {
            env = new.env()
            o = try( load(fn, env)[1], silent=TRUE )  
            if (!inherits(o, "try-error"))  return(env[[o]])
        }

        if ( filetype == "rds") {
            o = try( readRDS(fn), silent=TRUE )  
            if (!inherits(o, "try-error"))  return( o )
        }
        if ( filetype %in% c("rdz") ) {
            o = try( qs::qread(fn ), silent=TRUE)
            if (!inherits(o, "try-error"))  return( o )
        }

        if ( filetype %in% c("csv", "txt", "dat", "gz", "bz2") ) {
            # note: can read in URLs too 
            o = try( data.table::fread(fn, ... ), silent=TRUE )  
            if (!inherits(o, "try-error"))  return( o )
        }
        
        if ( filetype == "fst") {
            o = try(.Internal(unserialize(fst::decompress_fst(readBin(fn, "raw", file.size(fn))), refhook ) ))
            if (!inherits(o, "try-error"))  return( o )
        }
 
        if (inherits(o, "try-error")) {

            # in case filetype/file extension mismatches ... sequentially run most likely first
            message("Possible mismatch of filetype - file extension? or file corruption? 
                \nTrying every method in case. You might get strange warnings ...\n ")
            
            # rdata
            env = new.env()
            o = try( load(fn, env)[1], silent=TRUE )  
            if (!inherits(o, "try-error"))  return(env[[o]])

            # rds
            o = try( readRDS(fn), silent=TRUE )  
            if (!inherits(o, "try-error"))  return( o )

            # qs-zstd compressed files are called "*.rdz" in aegis
            o = try( qs::qread(fn ), silent=TRUE)
            if (!inherits(o, "try-error")) return( o )

            # csv
            o = try( data.table::fread(fn), silent=TRUE )  
            if (!inherits(o, "try-error")) return( o )

            # fst
            o = try(.Internal(unserialize(fst::decompress_fst(readBin(fn, "raw", file.size(fn))), refhook ) ), silent=TRUE )
            if (!inherits(o, "try-error")) return( o )

        }


        message( "Potential file name extension and filetype mismatch or file corruption. \n
            Filetype: ", filetype, " --- File extension: ", FN[["extension"]], "\nDropping into a debug session. Press Q or enter to quit"
        ) 

        browser()

        return()
    } 
    


    # data provided, so a save

    # create directories too
    dir.create( FN[["dirname"]], showWarnings=FALSE, recursive=TRUE )

    if (! inherits(fn, "character") )  stop( "fn needs to be a character")
    
    if (filetype %in% c("rda", "rdata" ) ) {
        save( data, file=fn, compress="zstd"   )
        return(fn)
    }


    if (filetype %in% c("rds") ) {
        saveRDS( data, file=fn, compress="zstd"  )
        return(fn)
    }
 
    if (filetype %in% c("fst") ) {

        compressor = NULL
        if ( compress =="fst-zstd" ) compressor = "ZSTD"
        if ( compress =="fst-lz4" )  compressor="LZ4"
        if (is.null(compressor)) compressor = "ZSTD"
        # message("Large files > 2^31-1 bytes can cause issues ... use qs based methods")
        writeBin( 
            fst::compress_fst( 
                x=base::serialize(data, NULL, ascii, xdr=TRUE), 
                compressor=compressor, 
                compression=round(compression_level*10, 0),   # note 0 - 100 (and not 0-9 as in RDS, etc)
                TRUE), 
            fn 
        ) 
        
        return(fn)
    }

    if (filetype %in% c("rdz") ) {
        preset = NULL
        if ( compress == "" ) preset = "high"
        if ( compress == "qs-custom" ) preset = "custom" # if custom, additional params are expected to be sent ...
        if ( is.null(preset)) {
            if ( compression_level %in% 9 ) preset = "archive"
            if ( compression_level %in% 7:8 ) preset = "high"   # zstd 4
            if ( compression_level %in% 4:6 ) preset = "balanced"  # lz4 1
            if ( compression_level %in% 1:3 ) preset = "fast"
            if ( compression_level %in% 0 )  preset = "uncompressed"
        }

        if (is.null(preset)) preset = "high"
 
        qs::qsave( x=data, file=fn, preset=preset, nthreads=floor(parallel::detectCores()-1L), ... )   # uses compress_level values from -50 to 22 for zstd and for lz > 1 (higher is less compressed ) .. confusing so use presets
        return(fn)
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

    stop( "Error: filetype and/or compression not recognized?" )


    if (0) {
        # test
        require(data.table)
        data(iris)

        fn = "~/tmp/iris_data.rdata"
        fn = "~/tmp/iris_data.rds"
        fn = "~/tmp/iris_data.rdz"

        read_write_fast( fn=fn, data =iris )
        o = read_write_fast( fn=fn )
        all(o == iris)

        str(o)

        # copy-save timings for a ~2GB file
        # 11:16:55 - 11:19:30 rds 2.2 GB 2.5 min
        # 11:20:05 - 11:22:58 fst 2.3 GB 2.9 min               <<-- good compression and slow .. but flexible random access 
        # 11:42:42 - 11:43:44 rdz 2.0 GB (high)  - 60-75 sec   <<-- fast with good compression 
        # 11:32:15 - 11:32:51 rdz 2.3 GB (balanced)  - 46 sec  <<-- fastest and good compresses
        # 11:35:15 - 11:36:00 rdz 3.7 GB (fast)  - 45 sec

        # test on a list:
        data(CO2)
        data(iris)
        tosave = list( CO2=CO2, iris=iris, fn=fn )
        attr(tosave, "test") = "test"

        fn2 = "~/tmp/test.rdz"
        read_write_fast( fn=fn2, data =tosave )
        o2 = read_write_fast( fn2 )

        for (i in 1:length(tosave)) print( all( tosave[[i]] == o2[[i]] ) )
        attr(tosave, "test") == attr(o2, "test") 


        fn3 = "~/tmp/test.rdata"
        read_write_fast( fn=fn3, data =tosave )
        o3 = read_write_fast( fn3 )

        for (i in 1:length(tosave)) print( all( tosave[[i]] == o3[[i]] ) )
        attr(tosave, "test") == attr(o3, "test") 

    }
    
}
