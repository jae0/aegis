
  



  cumsum.jae = function( x) {
    nv = length(x)
    for (i in 2:nv)  x[i] = sum(x[i-1], x[i], na.rm=T)
    x[x==0] = NA
    return(x)
  }
 


