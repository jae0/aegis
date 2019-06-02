nb2edge = function( W.nb, output="nodelist" ) {
  
  cW <- card(W.nb)
  nW <- length(W.nb)
  Wn = sum(cW) / 2;
  n1 = vector(mode="numeric", length=Wn);
  n2 = vector(mode="numeric", length=Wn);
  jj = 0;
  for (i in 1:nW) {
    nbi = W.nb[[i]]
    for (j in 1:length(nbi)) {
      if ( i < nbi[j] ) {
        jj = jj + 1;
        n1[jj] = i;
        n2[jj] = nbi[j];
      }
    }
  }
  
  if (output=="sparse.matrix") return( sparseMatrix(i=n1, j=n2, x=1, symmetric=TRUE) )
  if (output=="matrix") return( as.matrix(sparseMatrix(i=n1, j=n2, x=1, symmetric=TRUE)) )
  if (output=="nodelist") return( list( Wn=Wn, node1=n1, node2=n2 ) )

  return (list( Wn=Wn, node1=n1, node2=n2 )) # default to nodelist
}
