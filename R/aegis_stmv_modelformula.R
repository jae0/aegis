aegis_stmv_modelformula = function(p) {

    if ( !exists("stmv_global_modelformula", p)) {
      if (!exists("stmv_local_modelformula", p)) p$stmv_global_modelformula = formula( paste(
        p$stmv_variables$Y, '~ s(t, k=3, bs="ts") + s( tsd, k = 3, bs = "ts") ',
        ' + s( tmin, k = 3, bs = "ts") + s( tmax, k = 3, bs = "ts") + s( degreedays, k = 3, bs = "ts")  ',
        ' + s(log(z), k=3, bs="ts") + s( log(dZ), k=3, bs="ts") + s( log(ddZ), k=3, bs="ts")',
        ' + s(log(substrate.grainsize), k=3, bs="ts") ' ))
      }

    # local model options
    if (!exists("stmv_local_modelformula", p)) {
      if (p$stmv_local_modelengine =="twostep") {

        # p$stmv_local_modelformula = formula( paste(
        #   p$stmv_variables$Y, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
        #   ' + s(cos.w, sin.w, yr, bs="ts", k=16) ',
        #   ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=16, bs="ts") ' ) )  # required for the local TS modelling
        if (p$stmv_twostep_space == "gam") {
          if (!exists("stmv_local_modelformula_space", p)) p$stmv_local_modelformula_space = formula( paste(
          p$stmv_variables$Y, '~ s(log(z), k=3, bs="ts") + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s( log(z), plon, plat, k=27, bs="ts")  ') )
        }

        if (p$stmv_twostep_time == "gam") {
          if (!exists("stmv_local_modelformula_time", p)) p$stmv_local_modelformula_time = formula( paste(
            p$stmv_variables$Y, '~ s(yr, k=10, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
              ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(log(z), k=3, bs="ts") ' ,
              ' + s(cos.w, sin.w, yr, bs="ts", k=30) + s(log(z), plon, plat, k=30, bs="ts") '
            ) )  # required for the local TS modelling
          }

      }  else if (p$stmv_local_modelengine == "gam") {
        if (!exists("stmv_local_modelformula", p)) p$stmv_local_modelformula = formula( paste(
          p$stmv_variables$Y, '~ s(yr, k=5, bs="ts") + s(cos.w, k=3, bs="ts") + s(sin.w, k=3, bs="ts") ',
          ' + s(cos.w, sin.w, yr, bs="ts", k=16) ',
          ' + s(plon, k=3, bs="ts") + s(plat, k=3, bs="ts") + s(plon, plat, k=16, bs="ts") ' ) )
          # similar to GAM model .. this is the seed model
      }  else if (p$stmv_local_modelengine == "bayesx") {
        # bayesx families are specified as characters, this forces it to pass as is and
        # then the next does the transformation internal to the "stmv__bayesx"
        # alternative models .. testing .. problem is that SE of fit is not accessible?
        if (!exists("stmv_local_modelformula", p))  p$stmv_local_modelformula = formula( paste(
          p$stmv_variables$Y, ' ~ sx(yr,   bs="ps") + sx(cos.w, bs="ps") + s(sin.w, bs="ps") +s(z, bs="ps")',
          ' + sx(plon, bs="ps") + sx(plat,  bs="ps")',
          ' + sx(plon, plat, cos.w, sin.w, yr, bs="te") ' )
          # te is tensor spline
        )
      } else {
        # message( "The specified stmv_local_modelengine is not tested/supported ... you are on your own ;) ..." )
      }
    }
    return(p)
}
