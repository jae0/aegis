
read_write_fast = function ( file="", data=NULL, version=NULL, compress="zstd", compression_level=10, refhook=NULL, hash=TRUE)  {
    
    # nearly a drop-in replacement for base::readRDS and base::saveRDS -- expects file name as first argument
    # note: fst also has read_fst/write_fst functions but they specialize on data table/frames and random access ... 
    # use RDS as container for compressed file
    
    if (is.null(data)) {  # no data to save so, must be to load ... 
        
        if (is.character(data)) message( "First argument is file ('file') name and second is a data object ('data'), if not then use named arguments" )
          
        o = readRDS( file )
        
        if ( inherits(o, "raw") ) {  # not an RDS file so try to read as fst compressed file
            return( unserialize( fst:::fstdecomp(o), refhook ) )
        } else {
            return( o )
        }

    } else {   # save data as binary (raw compressed) using RDS as container

        if ( compress =="zstd" ) {
            saveRDS( object=fst:::fstcomp( serialize(data, NULL), "ZSTD", as.integer(compression_level), hash), 
                file=file, 
                compress=FALSE  
            ) 
        } else if ( compress =="lz4" ){
            saveRDS( object=fst:::fstcomp( serialize(data, NULL), "LZ4", as.integer(compression_level), hash), 
                file=file, 
                compress=FALSE 
            ) 
        } else {
            saveRDS( object=data, file=file, compress=compress ) # gzip(6), bzip2(9), xz(6) with (default levels of compression)
        }   

        return( message( "save completed", file )   )
    }        
   
}
