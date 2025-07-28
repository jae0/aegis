geometric_mean_sd = function(log_x, toget="mean") {
    # weighted geometric mean to handle zero-values
    # see aegis::summary_full() for explanations
    # input and output on log-scale
    nx = length(log_x) 
    log_x = log_x[ is.finite(log_x) ]
    np = length(log_x)
    wp = np/nx 
    log_out = switch( toget,
        mean = log(wp) + mean(log_x, na.rm=TRUE) ,    # i.e. log(wp) + sum( log( xp ) )/np 
        sd  = log(sqrt(wp)) + sd(log_x, na.rm=TRUE)
    )
    return(log_out)
}
