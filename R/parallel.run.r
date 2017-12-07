#'  Run a parallel process .. wrapper for snow/parallel. Expectation of all relevant parameters in a list 'p'.

parallel.run = function( FUNC, p, INDEX,  
  clusters=NULL, clustertype=NULL, clusterexport=NULL, rndseed=NULL, ... ) {

  require(parallel)

  nvars = length(INDEX)  # INDEX must be a list
  p$runs = expand.grid(INDEX)
  p$nruns = nrow( p$runs )
  p$runs_uid = do.call(paste, c(p$runs, sep="~"))
  
  if (is.null(clusters)) if (exists("clusters", p)) clusters = p$clusters
  if (is.null(clusters)) {
    clusters = "localhost"
    message( "'clusters' were not defined, using serial mode" )
  }  
  
  if (is.null(clustertype)) if (exists("clustertype", p)) clustertype=p$clustertype 
  if (is.null(clustertype)) {
    clustertype = "PSOCK"
    message( "'clustertype' was not defined, using PSOCK connections as default." )
  }
    
  if (is.null(rndseed)) if (exists("rndseed", p)) rndseed=p$rndseed 
  if (is.null(rndseed)) {
      rndseed = 1
      message( "'rndseed' was not defined, using rndseed=1 as the default." )
  }
    
  message( "The processes are being run on:")
  message(  paste( unlist( clusters), collapse=" ") )

  out = NULL
  if ( length(clusters) == 1 | nruns==1 ) {
    out = suppressMessages( FUNC( p=p, ... ) )
  } else if ( nruns < length(clusters) ) {
    clusters = sample( clusters, nruns )  # if very few runs, use only what is required
  }
  
  if ( length(clusters) > 1 ) {
    cl = makeCluster( spec=clusters, type=clustertype ) # SOCK works well but does not load balance as MPI
    RNGkind("L'Ecuyer-CMRG")  # multiple streams of pseudo-random numbers.
    clusterSetRNGStream(cl, iseed=rndseed )
    if ( !is.null(clusterexport)) clusterExport( cl, clusterexport )
    uv = unique(p$runs_uid)
    uvl = length(uv)
    lc = length(clusters)
    lci = 1:lc
    ssplt = list()
    for(j in 1:uvl) ssplt[[j]]  = which(p$runs_uid == uv[j])
    clustertasklist = rep(list(numeric()),lc)
    if (uvl>lc) {
      for(j in 1:uvl) {
        k=j
        if(j>lc) k = j%%lc+1
        clustertasklist[[k]] <- c(clustertasklist[[k]],ssplt[[j]])
      }
    }
    ssplt = NULL
    out = suppressMessages( clusterApply( cl, clustertasklist, FUNC, p=p, ... ) )
    stopCluster( cl )
  }
  return(  out  )
}
