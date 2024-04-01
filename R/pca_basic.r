pca_basic = function(cm=NULL, indat=NULL, rotate="none",  nfactors=2, ev_template=NULL, save_fn=NULL, ...) {
  # see https://stats.stackexchange.com/questions/59213/how-to-compute-varimax-rotated-principal-components-in-r

  pca_results = NULL

  if (is.null(cm)) {
    if( !is.null(save_fn)) {
      if (file.exists(save_fn)) load( save_fn)
      return(pca_results)
    }
  }

  varnames = colnames(indat)
    if (!all(varnames %in% rownames(cm) ))  {
      message( "corelation matrix, cm does not have all the variables in varnames")
      message( setdiff(rownames(cm), varnames) )
      stop()
    }

  cm = cm [varnames,]
  cm = cm [,varnames]

  # usually svd is done on the indat data matrix but here we use it on the cor/covar matrix so essentially eignanalysis
  # if used on data then it would have to be scaled to nobs : s = svd(indat/sqrt(nobs-1))
  s = svd( cm ) 
  eigenvalues = s$d
  eigenvectors = s$v

  pcnames = paste("PC", 1:length(eigenvalues), sep="")

  names(eigenvalues) = pcnames

  for ( kk in 1:length(eigenvalues) ) {
    if (!is.null(ev_template)) {
      if ( cor( eigenvectors[,kk], ev_template[,kk] )  < 0 ) eigenvectors[,kk] = -1 * eigenvectors[,kk]
    } else {
      gg = abs(eigenvectors[,kk])
      uu = which.max( gg  )
      if ( eigenvectors[uu,kk] < 0 ) eigenvectors[,kk] = -1 * eigenvectors[,kk]  # force max value to be positive
    }
  }
  rownames(eigenvectors) = varnames
  colnames(eigenvectors) = pcnames
 
	loadings = eigenvectors  %*% diag(sqrt( eigenvalues ))    # Loadings are eigenvectors scaled by the square roots of the respective eigenvalues
  colnames(loadings) = pcnames

  if (0) {
    # older method:
    matrix.multiply = function (x, y, nfac=2){ 
        ndat = dim(x)[1]
        z = matrix(0, nrow=ndat, ncol = nfac)
        for (j in 1:nfac) { 
          for (i in 1:ndat) { 
            z[i,j] = sum ( x[i,] * t(y[,j]), na.rm=T )
          } 
        }
        return (z)
    }  

    scores = matrix.multiply (m, s$v)  # i.e., b %*% s$v  .. force a multiplication ignoring NAs
    # m[which(!is.finite(m))] = 0
    # scores = m %*% s$v  # i.e., b %*% s$v  .. force a multiplication ignoring NAs

  }

  # error .. these are the usual (unscaled) scores
  scores = indat %*% loadings  # again, scaled by eigenvectors ( unscaled scores give "distance biplots" )
  colnames(scores) = pcnames

  # these are scaled scores
  # essentially identical to (unscaled) scores, except that their magnitudes are smaller, but correlation is 100%
  # usually used for plotting (in biplots..  scaled by eigenvectors ) as in R package factominer
  scores_old = indat %*% t(pracma::pinv(loadings ))  
  colnames(scores_old) = pcnames

  total_variance = length(eigenvalues)  # note forcing this to be for scaled and standardized matrices .. ie. .. not for covariance

  pca_results = list(
    cm = cm,
    indat = indat,
    rotate=rotate,
    eigenvalues=	eigenvalues,
    eigenvectors=	eigenvectors,
    loadings=loadings,
    scores= scores,
    scores_old=scores_old,
    variance = eigenvalues,
    variance_percent = round(eigenvalues  / total_variance * 100, 2) ,
    total_variance = total_variance
  )

	if (rotate!="none")  {  #
	  if (rotate=="varimax") rot = varimax( loadings[,1:nfactors], ... )  #  normalize=FALSE, eps=1e-14)
	  if (rotate=="promax")  rot = promax(  loadings[,1:nfactors], ... )  #  normalize=FALSE, eps=1e-14)

    rotated_scores =  indat  %*% t(pracma::pinv(rot$loadings))
    colnames( rotated_scores ) = paste( "RC", 1:nfactors, sep="" )
    # rotated_scores_alt = scores[,1:nfactors] %*% rot$rotmat # testing: should be the same as rotated_scores
    pca_results$rotated_loadings = rot$loadings
    colnames(pca_results$rotated_loadings) = paste( "RC", 1:nfactors, sep="" )
    pca_results$rotated_scores = rotated_scores
    pca_results$rotated_variance = colSums(rot$loadings^2)
    pca_results$rotated_variance_percent = round( pca_results$rotated_variance /  total_variance * 100, 2)
    pca_results$rotation = rot$rotmat
    pca_results$rotation_id = rotate
	}

  if ( !is.null(save_fn)) save( pca_results, file=save_fn, compress=TRUE )

	return(  pca_results )



    # testing in R: (https://stats.stackexchange.com/questions/276645/arrows-of-underlying-variables-in-pca-biplot-in-r)
    X=iris[,1:4]
    CEN = scale(X, center = T, scale = T) # Centering and scaling the data
    PCA = prcomp(CEN)
    pca = pca_basic( cm=cor(CEN), indat=CEN )

    # EIGENVECTORS:
    (evecs.ei = eigen(cor(CEN))$vectors)       # Using eigen() method
    (evecs.svd = svd(CEN)$v)                   # PCA with SVD...
    (evecs = prcomp(CEN)$rotation)             # Confirming with prcomp()
    (pca$eigenvectors)

    # EIGENVALUES:
    (evals.ei = eigen(cor(CEN))$values)        # Using the eigen() method
    (evals.svd = svd(CEN)$d^2/(nrow(X) - 1))   # and SVD: sing.values^2/n - 1
    (evals = prcomp(CEN)$sdev^2)               # with prcomp() (needs squaring)
    ( pca$eigenvalues)

    # SCORES:
    scr.svd = svd(CEN)$u %*% diag(svd(CEN)$d)  # with SVD
    scr = prcomp(CEN)$x                        # with prcomp()
    scr.mm = CEN %*% prcomp(CEN)$rotation      # "Manually" [data] [eigvecs]
    pca$scores

    # LOADINGS:
    loaded = evecs %*% diag(sqrt(evals))  # [E-vectors] [sqrt(E-values)]
    pca$loadings

}
