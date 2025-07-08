
    filenames = function( filename_full=NULL, basename=NULL, extension=NULL, location=NULL, force_lower_case_extension=FALSE, clean=FALSE ) {
    
        # get filename extension: lifted from https://stackoverflow.com/questions/7779037/extract-file-extension-from-file-path asperamanka's solution

        if (is.null(filename_full)) {
            if (!is.null(basename)) {
                if (!is.null(extension)) {
                    filename_full = paste(basename, extension, sep=".")
                } else {
                    filename_full = basename
                }
            }
            if (!is.null(location)) {
                filename_full = file.path( location, filename_full )
            }
        }

        if (clean) {
            # replace spaces and strange characters to _ and convert all to lowercase
            # filename_full = stringr::str_replace_all(filename_full, "[[:space:]]+", "_")
            filename_full = tolower( stringr::str_replace_all(filename_full, "[^~|.\\/a-zA-Z0-9]", "_") )
            message("Spaces and symbols will be converted to _ ." )
        }

        # .. deconstruct 
        out = list()
        out[["fullname"]] = filename_full
        out[["dirname"]]  = dirname(out[["fullname"]])
        out[["basename"]] = basename(out[["fullname"]])
        out[["extension"]] = sub( "^(.*\\.|[^.]+)(?=[^.]*)", "", out[["basename"]], perl = TRUE) 
        out[["corename"]] = filename_base = sub( "(.*)\\..*$", "\\1", out[["basename"]] )

        if (force_lower_case_extension) {
            out[["extension"]] = tolower(out[["extension"]])
            out[["basename"]] = paste(out[["corename"]], out[["extension"]], sep=".")
            out[["fullname"]] = file.path( out[["dirname"]], out[["basename"]] )
            message("File type/extension will be in lower case. If you do not want this behaviour, set: \n  force_lower_case_extension=FALSE \nVerify that the current target is OK: \n", out[["fullname"]])
        }

        return(out)

        if (0) {
            # tests :
            filenames( "C:/Data/SCRIPTS/R/TextMining/myData/test.rds" )  
            filenames( "C:\\Data\\SCRIPTS\\R\\TextMining\\myData/test.rds" )  
            filenames( "/home/jae/tmp/TextMining/myData/test.rds" )  
            filenames( "/home/jae/tmp/TextMining/myData/test.#d_" )   # strange characters ok
            filenames( "/home/jae/tmp/TextMining/myData/test._d*" )  # strange characters ok
            filenames( "/home/jae/tmp/TextMining/myData/test!._d%" )  # strange characters ok
            filenames( "/home/jae/tmp/Text Mining/my Data/test!._d%", clean=TRUE )  
        }

    }
