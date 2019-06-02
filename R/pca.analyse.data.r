
pca.analyse.data = function( X, t0=NULL, t1=NULL,
  fname="sorted.anomalies", title="Years", vars=NULL, newnames=NULL,
  colscheme="redgreen", addscores=TRUE, sortdecreasing=FALSE, scaledata=TRUE, plotdata=TRUE) {

  if (!is.null(vars)) X = X[, vars ]
  if (!is.null(newnames) ) names(X) = newnames

  if (!is.null(t0) & !is.null(t1)) {
    X = X[ which( rowSums( X, na.rm=T) !=0 ) ,]
    X = X[ which( as.numeric(rownames(X)) >= t0 ) ,]
    X = X[ which( as.numeric(rownames(X)) <= t1 ) ,]
  }

  if (scaledata) X = scale( X, center=TRUE, scale=TRUE )

  yvals=rownames(X)
  vars =colnames(X)
  years= as.numeric(yvals)
  yrange = range(years)

  of1 = paste(fname, "PC1.pdf", sep=".")
  of2 = paste(fname, "PC2.pdf", sep=".")
  of3 = paste(fname, "anomalies.pdf", sep=".")

  cm = cor( X, use="pairwise.complete.obs" ) # set up a correlation matrix ignoring NAs
  cm[ is.na(cm) ] = 0

  s = svd(cm)  # eigenanalysis via singular value decomposition
  evec = s$v # eigenvectors
  eval = s$d # eigenvalues
  
  # i.e., X %*% evec  .. force a multiplication ignoring NAs
  nfac = 2 
  ndat = dim(X)[1]
  scores = matrix(0, nrow=ndat, ncol = nfac)
  for (j in 1:nfac) { 
    for (i in 1:ndat) { 
      scores[i,j] = sum ( X[i,] * t(evec[,j]), na.rm=T )
    } 
  }

  x = cbind( scores[,1] / sqrt(eval[1] ), scores[,2] / sqrt( eval[2]) )
  y = cbind( evec[,1] * sqrt(eval[1] ) , evec[,2] * sqrt( eval[2]) )

  outscores = data.frame(x)
  outscores$yr = as.numeric(yvals)

  X.stats = list( id=yvals, vars=vars, correlation.matrix=cm,
    eigenvectors=evec, eigenvalues=eval, projections.id=x, projections.vars=y )

  print( str( X.stats))

  if (plotdata) {

    par(cex=2, lty=2)

    plot.new()
    plot( yvals, x[,1], xlab="Years", ylab=paste("PCA1 (", round(eval[1]/sum(eval)*100, 1), "%)", sep="") )
    lines( lowess(yvals, x[,1], f=1/5 ) )
    dev.print ( pdf, file=of1, width=12, height=8 )
    dev.off()

    plot.new()
    plot( yvals, x[,2], xlab="Years", ylab=paste("PCA2 (", round(eval[2]/sum(eval)*100, 1), "%)", sep="" ) )
    lines( lowess(yvals, x[,2], f=1/5) )
    dev.print ( pdf, file=of2, width=12, height=8 )
    dev.off()

    # form residual variations figure based upon a sorting of ordination scores
    varloadings = NULL
    varloadings = as.data.frame( cbind( y, vars ) )

    q = as.data.frame( t( as.matrix( X ) ) )
    q$vars = rownames( q )
    q = merge( x=q, y=varloadings, by.x="vars", by.y="vars", all=T )
    ordered = sort( as.numeric( as.character( q$V1 ) ), index.return=T, decreasing=sortdecreasing )
    qq = q[ ordered$ix, ]

    if (addscores) {
      varnames = paste(qq$vars, " {", round(as.numeric(as.character(qq$V1)),2), ", ",
                 round(as.numeric(as.character(qq$V2)),2), "}", sep="")
    } else {
      varnames = qq$vars
    }

    qq$vars = qq$V1 = qq$V2 = NULL
    qq = as.data.frame( t( qq ) )
    colnames( qq ) = varnames
    Z = as.matrix( qq )

    a = max( abs( min(Z, na.rm=T) ), abs( max( Z, na.rm=T) ) )
    br = seq( -a, a, .1)

    plot.new()
    par( mai=c(2, 6, 1, 0.5), cex=2 )

    if (colscheme=="rainbow")  {
      cols=rainbow(length(br)-1, start=0, end=1/3)
    } else if (colscheme=="redgreen") {
      cols=rev(green.red(length(br)))
    } else if (colscheme=="bluered") {
      cols=rev(blue.red(length(br)))
    } else if (colscheme=="heat") {
      cols=rev(heat.colors(length(br)-1))
    } else {
      cols=colscheme
    }

    image(z=Z, x=years, breaks=br, col=cols, xlab=title, ylab="", axes=F )

    for (i in seq(range(yrange)[1], range(yrange)[2], by=10)) abline( v=i-0.5, col="slategray")
    for (i in seq(range(yrange)[1], range(yrange)[2], by=1)) abline( v=i-0.5, col="slategray1")

    z = 1/(dim(Z)[2] - 1)
    for (i in seq(0, 1, by=z*10)) abline( h=i-(z/2), col="slategray")
    for (i in seq(0, 1, by=z)) abline( h=i-(z/2), col="slategray1")

    par(las=1)
    axis( 1 )

    vars = colnames(Z)
    par(cex=1.4)
    axis( 2, at=seq(0,1,length=length(vars)), labels=vars)
    write.table(Z, file="anomalies.dat", quote=F, sep=";")
    dev.print (pdf, file=of3, width=20, height=20)
    dev.off()
  }

  return( X.stats )
}
