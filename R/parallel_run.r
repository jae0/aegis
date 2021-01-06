#'  Run a parallel process .. wrapper for snow/parallel. Expectation of all relevant parameters in a list 'p'.

parallel_run = function( p, FUNC=NULL, runindex=NULL,
  clusters=NULL, clustertype=NULL, clusterexport=NULL, clusterLoadBalance=TRUE,
  rndseed=NULL, verbose=FALSE, ... ) {

  require(parallel)

  if (is.null(runindex)) if (exists("runindex", p)) runindex = p$runindex
  if (is.null(runindex)) stop( "'runindex' needs to be defined" )

  nvars = length(runindex)  # runindex must be a list
  p$runs = expand.grid(runindex, stringsAsFactors=FALSE, KEEP.OUT.ATTRS=FALSE)
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
    if (verbose) message( "'clustertype' was not defined, using PSOCK as a default." )
  }

  if (!is.null(rndseed)) p$rndseed = rndseed
  if (!exists("rndseed", p))  {
      p$rndseed = 1
      if (verbose) message( "'rndseed' was not defined, using rndseed=1 as the default." )
  }

  if (is.null(FUNC)) return(p)  # FUNC is NULL means no running just return params

  if (verbose) {
    message( "The processes are being run on:")
    message(  paste( unlist( p$clusters), collapse=" ") )
  }

  out = NULL

  if ( length(p$clusters) == 1 | p$nruns==1 ) {

    out = FUNC( p=p, ... )
    return ( out )

  } 

  if (p$nruns < length(p$clusters)) p$clusters = sample(p$clusters, p$nruns)

  if ( p$clustertype=="FORK" ) p$clusters = length( p$clusters)

  cl = makeCluster( spec=p$clusters, type=p$clustertype ) # SOCK works well but does not load balance as MPI
  RNGkind("L'Ecuyer-CMRG")  # multiple streams of pseudo-random numbers.
  clusterSetRNGStream(cl, iseed=p$rndseed )
  if ( !is.null(clusterexport)) clusterExport( cl, clusterexport )

  uv = unique(p$runs_uid)
  uvl = length(uv)
  lc = length(p$clusters)
  lci = 1:lc
  ssplt = list()

  if (clusterLoadBalance) {
    for (j in 1:uvl) ssplt[[j]]  = which(p$runs_uid == uv[j])
    nchunks =  max( length(cl), ceiling( uvl / length(cl) ) ) 
    clustertasklist = rep(list(numeric()),nchunks)
    for (j in 1:uvl) {
      k=j
      if (j>nchunks) k = j%%nchunks+1
      clustertasklist[[k]] = c(clustertasklist[[k]], ssplt[[j]])
    }
    ssplt = NULL
    out = clusterApplyLB( cl, clustertasklist, FUNC, p=p, ... )
    
  } else {
    for (j in 1:uvl) ssplt[[j]]  = which(p$runs_uid == uv[j])
    clustertasklist = rep(list(numeric()),lc)
    for (j in 1:uvl) {
      k=j
      if (j>lc) k = j%%lc+1
      clustertasklist[[k]] = c(clustertasklist[[k]], ssplt[[j]])
    }
    ssplt = NULL
    out = clusterApply( cl, clustertasklist, FUNC, p=p, ... )
  }

  stopCluster( cl )
   
  return ( out )


}
