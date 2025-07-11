convert_file_format = function( convert_from="rds", convert_to="rdz", directory=NULL, fn=NULL, delete_original=FALSE, recursive=FALSE, ask=TRUE, dry_run=TRUE ) {

    if (!is.null(fn)) {
        if (file.exists(fn)) {
            message(fn)

            if (dry_run) return()

            if (ask) {
                a = readline(prompt="\nPress y and enter to continue, anything else to skip, CTRL-c to stop:  ")
                if (a!="y") return()
            }

            o = read_write_fast(fn=fn)

            if (is.null(o)) {

                message(fn, " looks like an empty file ... skipping\n\n")
                return()
            
            } else {

                FN = filenames( fn, extension=convert_to )
                
                read_write_fast( data=o, fn=FN[["fullname_new"]] )
                message( "\nConverting: ", fn, " -> \n  ", FN[["fullname_new"]], "\n") 
                o = NULL; gc() 

                if (delete_original) {
                    if (convert_from != convert_to) {
                        # don't want to delete newly created file .. :)
                        message("Deleting: " )
                        file.remove(fn)
                    }
                }
            }
        }
        return("\n")
    }

    if (!is.null(directory)) {
        fns = list.files( 
            directory, 
            pattern=paste0("*.", convert_from, "$"), 
            all.files=FALSE,
            full.names=TRUE,
            recursive=recursive,
            ignore.case=TRUE
        ) 
        
        for (fn in fns) {
            convert_file_format( convert_from=convert_from, convert_to=convert_to, fn=fn, delete_original=delete_original, ask=ask, dry_run=dry_run)
        }
        return("done")
    }

    return("complete")

    if (0) {
        
        # test

        fn2 = "~/tmp/iris_data.rdz"  # rdz is qs::qsave() with zstd compression
        read_write_fast( fn=fn, data =iris )
        o = read_write_fast( fn=fn )
        str(o)

        convert_file_format( directory="~/tmp", convert_from="rdz", convert_to="rdata", delete_original=FALSE, recursive=FALSE )
        fn2 = filenames(fn, extension="rdata")
        o2 = read_write_fast( fn=fn2[["fullname_new"]] )

        all( o == o2 )

        convert_file_format( directory="~/tmp", convert_from="rdz", convert_to="rds", delete_original=FALSE, recursive=FALSE )
        fn3 = filenames(fn, extension="rds")
        o3 = read_write_fast( fn=fn3[["fullname_new"]] )

        all( o == o3 )

        list.files("~/tmp/")

    }

}