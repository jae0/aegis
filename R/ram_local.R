ram_local = function( returnvalue="ram", ram_main=NULL, ram_process=NULL ) {
  os = Sys.info()[["sysname"]]
  if (os == "Windows"){
    x = try( memory.size(max = FALSE))
    if ( "try-error" %in% class(x) ) {
      x <- system2("wmic", args =  "OS get FreePhysicalMemory /Value", stdout = TRUE)
      x <- x[grepl("FreePhysicalMemory", x)]
      x <- gsub("FreePhysicalMemory=", "", x, fixed = TRUE)
      x <- gsub("\r", "", x, fixed = TRUE)
      out = as.integer(x)
    }
  }

  if (os=="Linux") {
    x = system2('free', args='-g', stdout=TRUE)
    y = x[ grepl("total", x) ]
    y = unlist( strsplit( y, "[[:space:]]+") )
    y[1] = "dummy"

    z = x[ grepl("Mem", x) ]
    z = unlist( strsplit( z, "[[:space:]]+") )
    z[1] = NA
    z = as.numeric(z)
    names(z) = y
    out = z["total"]
  }

  if (returnvalue=="ram") return (out)   

  if (returnvalue=="ncores") {
    ncores_total = parallel::detectCores() 
    ncores_required = floor( (out - ram_main) / ram_process ) 
    return( min( ncores_total, ncores_required ) )
  }
}
