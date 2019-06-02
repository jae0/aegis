
meanweights_by_strata = function( set, StrataID, yrs, fillall=FALSE, annual_breakdown_only=FALSE ) {
  # find mean weight for each stratum and year .. fill where possible if required
  strata=data.frame( order=1:length(StrataID) )
  strata$StrataID = as.character( StrataID )
  out = list( StrataID=StrataID )
  out$meanweights = matrix(NA, ncol=length(yrs), nrow=length(StrataID), dimnames=list( StrataID, yrs) )
  out$meanweights_overall = rep(NA, length=length(yrs))
  for (y in 1:length(yrs) ) {
    i = which( set$yr==yrs[y] )
    # aggregate the weight at each stratum and then average
    if (length(i) < 1) next()
    rowindex = 1:length(i)
    jj = which (is.finite( set[i,"totwgt"] ) & is.finite(set[i,"totno"]) )
    out$meanweights_overall[y] = sum(set[i[jj],"totwgt"]) / sum(set[i[jj],"totno"])
    meanweight_crude = set[i,"totwgt"] / set[i,"totno"]
    wmeans = aggregate(rowindex ~ StrataID, set[i,c("StrataID", "totno", "totwgt")],
      function(rn) weighted.mean(meanweight_crude[rn], set[i[rn], "totwgt"], na.rm=TRUE )) # weighted mean
    oo = merge( strata, wmeans, by="StrataID", all.x=TRUE, all.y=FALSE, sort=FALSE)
    oo = oo[order(oo$order),]
    out$meanweights[,y] = oo$rowindex
  }

  out$meanweights_timeaverage = apply( out$meanweights, 1, mean, na.rm=TRUE)

  if (fillall) {
    for (y in 1:length(yrs) ) {
      kk = which(!is.finite(out$meanweights[,y]))
      if (length(kk) > 0) out$meanweights[kk,y] = out$meanweights_timeaverage[kk]
    }
    ll = which(!is.finite(out$meanweights))
    if (length(ll) > 0) out$meanweights[ll] = out$meanweights_overall
    ll = which(!is.finite(out$meanweights))
    if (length(ll) > 0) out$meanweights[ll] = median(out$meanweights, na.rm=TRUE)
  }

  if ( annual_breakdown_only ) return( out$meanweights)

  return(out)
}
