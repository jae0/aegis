
aegis_lookup_mapparams = function( DS="datarange", vn ){

  if (DS=="datarange") {
    datarange = NULL
    if ( vn == "ca1" ) datarange = seq(-3, 3, length.out=100)
    if ( vn == "ca2" ) datarange = seq(-3, 3, length.out=100)
    if ( vn == "pca1" ) datarange = seq(-0.4, 0.6, length.out=100)
    if ( vn == "pca2" ) datarange = seq(-0.5, 0.6, length.out=100)
    if ( vn == "smr" )  datarange = seq(0.006, 0.019, length.out=100)
    if ( vn == "mr" )  datarange = seq(0, 80000, length.out=100)
    if ( vn == "Npred" )  datarange = seq(75, 170, length.out=100)

    if ( grepl("rsquared", vn, ignore.case=TRUE) ) datarange = seq(0.1, 0.99, length.out=100)
    if ( grepl("phi", vn) ) datarange = seq(0.5, 50, length.out=100)
    # if ( grepl("range", vn) ) datarange = seq(0.5, 100, length.out=100)
    if ( grepl("nu", vn) ) datarange = seq(0.1, 3, length.out=100)
    return(datarange)
  }

  # -------------------------

#  if (DS=="color.code") {
#    color.code = "blue.black"
#    return (color.code)
#  }

}
