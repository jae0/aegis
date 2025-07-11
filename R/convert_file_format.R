convert_file_format = function( convert_from="rds", convert_to="rdz", directory=NULL, fn=NULL, delete_original=FALSE, recursive=FALSE, ask=TRUE, dry_run=TRUE, skip_if_already_exists=TRUE ) {

    if (!is.null(fn)) {
        if (!file.exists(fn))  stop("file not found")

        FN = filenames( fn, extension=convert_to )

        message(fn)

        if (file.exists(FN[["fullname_new"]]) ) {
            if (skip_if_already_exists) {
                if (delete_original) {
                    if (convert_from != convert_to) {
                        # don't want to delete newly created file .. :)
                        message("Deleting: " )
                        file.remove(fn)
                    }
                }
                return("\nSkipping as destination file already exists.\n")
            }

        } 
        
        if (dry_run) {
            return("dry run: no changes made")
        }

        if (ask) {
            message("\nLoad source file?\n")
            a = readline(prompt="\nPress y and enter to continue, anything else to skip, CTRL-c to stop, to stop message send ask=FALSE:  ")
            if (a!="y") return()
        }

        o = read_write_fast(fn=fn)

        if (is.null(o)) {

            return("\nLooks like an empty file ... skipping\n\n")
        
        } else {
            if (ask) {
                message("\nCreate new file?\n")
                a = readline(prompt="\nPress y and enter to continue, anything else to skip, CTRL-c to stop, to stop message send ask=FALSE:  ")
                if (a!="y") return()
            }
             
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
        return("done")
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

        if (dry_run) {
            message("\nDry run: showing files that would be changed. To make real changes, send: dry_run=FALSE \n")
        }

        for (fn in fns) {

            convert_file_format( convert_from=convert_from, convert_to=convert_to, fn=fn, delete_original=delete_original, ask=ask, dry_run=dry_run, skip_if_already_exists=skip_if_already_exists)
        }

        if (dry_run) {
            message("\nDry run: showing files that would be changed. To make real changes, send: dry_run=FALSE \n")
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


        convert_file_format(directory="/home/bio.data/bio.snowcrab", convert_from="rdata", delete_original=TRUE, recursive=TRUE, ask=FALSE, dry_run=FALSE, skip_if_already_exists=TRUE)
    }

}