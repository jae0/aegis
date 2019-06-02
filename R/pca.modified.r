 pca.modified = function (b) {
    id = rownames(b)
    vars = colnames(b)
    corel = cor(b, use="pairwise.complete.obs")
    corel[is.na(corel)] = 0
    s = svd(corel)
    evec = s$v
    eval = s$d
    if (0) {
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
        scores = matrix.multiply (b, s$v)  # i.e., b %*% s$v  .. force a multiplication ignoring NA
        scores = cbind(scores[,1]/sqrt(eval[1]), scores[,2]/sqrt(eval[2]) )
    }
    b[which(!is.finite(A))] = 0  # TODO:: prob better to try to estimate missing values from linear models ...
    scores = b %*% s$v  %*% (1/sqrt(eval)) # i.e., b %*% s$v  .. force a multiplication ignoring NA
    loadings = cbind(evec[,1]*sqrt(eval[1]) , evec[,2]*sqrt(eval[2]) )
    X11()
    biplot(scores, loadings, var.axes=T, col=c("blue", "red"), ylabs=vars)
    X11()
    plot(loadings)
    text(loadings[,1], loadings[,2], vars)
  }

