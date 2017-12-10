#'  Run a parallel process .. wrapper for snow/parallel. Expectation of all relevant parameters in a list 'p'.

parallel_run = function( p,  FUNC, runindex=NULL,   
  clusters=NULL, clustertype=NULL, clusterexport=NULL, rndseed=NULL, verbose=FALSE, ... ) {

  require(parallel)

  if (is.null(runindex)) if (exists("runindex", p)) runindex = p$runindex
  if (is.null(runindex)) {
    stop( "'runindex' was not defined" )
  }  

  nvars = length(runindex)  # runindex must be a list
  p$runs = expand.grid(runindex)
  p$nruns = nrow( p$runs )
  p$runs_uid = do.call(paste, c(p$runs, sep="~"))
  
  if (!is.null(clusters)) p$clusters = clusters
  if (!exists("clusters", p)) {
    p$clusters = "localhost"
    if (verbose) message( "'clusters' were not defined, using serial mode" )
  }  
  
  if (!is.null(clustertype)) p$clustertype = clustertype 
  if (!exists("clustertype", p)) {
    p$clustertype = "PSOCK"
    if (verbose) message( "'clustertype' was not defined, using PSOCK connections." )
  }
    
  if (!is.null(rndseed)) p$rndseed = rndseed 
  if (!exists("rndseed", p))  {
      p$rndseed = 1
      if (verbose) message( "'rndseed' was not defined, using rndseed=1 as the default." )
  }
    
  if (verbose) {
    message( "The processes are being run on:")
    message(  paste( unlist( p$clusters), collapse=" ") )
  }
  
  out = NULL
  if ( length(p$clusters) == 1 | p$nruns==1 ) {
    out = suppressMessages( FUNC( p=p, ... ) )
  } else if ( p$nruns < length( p$clusters ) ) {
    p$clusters = sample( p$clusters, p$nruns )  # if very few runs, use only what is required
  }
  
  if ( length(p$clusters) > 1 ) {
    cl = makeCluster( spec=p$clusters, type=p$clustertype ) # SOCK works well but does not load balance as MPI
    RNGkind("L'Ecuyer-CMRG")  # multiple streams of pseudo-random numbers.
    clusterSetRNGStream(cl, iseed=p$rndseed )
    if ( !is.null(clusterexport)) clusterExport( cl, clusterexport )
    uv = unique(p$runs_uid)
    uvl = length(uv)
    lc = length(p$clusters)
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
