

  plot.ordination = function( X, type="pca", fn=tempfile()  ) {

    if (type == "pca") {
      pdf( file=fn, pointsize=8 )
        plot( X, pch=".", col="red" , xlab="PCA1", ylab="PCA2" )
        text( X, labels= rownames(X), cex=0.6 )
      dev.off()
      system( paste( "inkscape", fn ) )
    }

    return (fn) 
  }


