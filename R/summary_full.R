
summary_full = function(x) {
    # see justification here: http://www.arpapress.com/Volumes/Vol11Issue3/IJRRAS_11_3_08.pdf
    # summarized: https://stats.stackexchange.com/questions/234816/dealing-with-zeroes-while-using-geometric-mean
    # xz = x[x<=0]; nz = length(xz)
    # xp = x[x>0];  np = length(xp)
    # wp = np / (np +nz)  # fraction positive valued
    # xm = [np / (np+nz) * product(xp)^(1/np)] + [nz / (np+nz) * product(xz)^(1/np)]  #weighted sum
    # weighted sums ("tri-geometrical" .. 0, -, +) : product(xz)=0 leaving:
    # xm = [np / (np+nz) * product(xp)^(1/np)]
    # log(xm) = log(np/(np+nz)) + sum( log( xp ) )/np  # using logs
    # xm =  np/(np+nz) * exp( sum( log( xp ) )/np )

    # var(xm) =  np/(np+nz) * var(log(xp)) + nz/(np+nz) * var(log(xz)) # second term is also zero ...
    # xv =  np/(np+nz) * var(log(xp)) 
    # xSD = sqrt(np/(np+nz)) * SD( log(xp)) )

    # simple arithmetic ...
    amean = mean(x, na.rm=TRUE)
    asd = sd(x, na.rm=TRUE)
    aci = quantile(x, probs=c(0.05, 0.95), na.rm=TRUE)

    # geometric on positive valued weighted
    xl = log(x[x>0])
    np = length(xl)
    nx = length(x)
    nz = nx - np
    wp = np/nx 

    mp = mean(xl, na.rm=TRUE)
    sp = sd(xl, na.rm=TRUE)
    nzci = exp( quantile(xl, probs=c(0.05, 0.95), na.rm=TRUE) )

    xml = log(wp) + mp    # i.e. log(wp) + sum( log( xp ) )/np 
    xsl = log(sqrt(wp)) + sp
    
    xm = exp(xml)
    xs = exp(xsl)

    if (!is.finite(xm)) xm = 0
    if (!is.finite(xs)) xs = 0
    
    ci = exp(xml + xsl*c(-1.96, 1.96)) # assuming lognormal
    i = which(!is.finite(ci)) 
    if(length(i)>0) ci[i] = 0
    

    return( c(
        mean_arithmetic =amean, 
        sd_arithmetic =asd, 
        lb05_arithmetic =min(aci), 
        ub95_arithmetic =max(aci),
        mean_geometric = xm, 
        sd_geometric = xs, #multiplicative
        lb05_geometric =min(ci), 
        ub95_geometric =max(ci),
        mean_nonzero =exp(mp), 
        sd_nonzero =exp(sp), 
        lb05_nonzero =min(nzci), 
        ub95_nonzero =max(nzci),
        n = nx,
        npostive = np,
        nzero = nz
    ) )

    #test:
    u = CO2$uptake
    summary_full(u)

    u = c(CO2$uptake, rep(0, 10))
    summary_full(u)

    # hail marie: min value is "detection limit"
    u = c(CO2$uptake, rep(0, 10))
    u[u<=0] = min(u[u>0])
    summary_full(u)
 
    # hail marie 2: min value/10 is "detection limit" ... magnifies variance 
    u = c(CO2$uptake, rep(0, 10))
    u[u<=0] = min(u[u>0])/10
    summary_full(u)

}
