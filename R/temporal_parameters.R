
temporal_parameters = function( p=list(), ... ) {

  p = parameters_add(p, list(...)) # add passed args to parameter list, priority to args

  # ----------------------------------
  # determine problem's temporal dimensionality (for outputs)
  
  if ( p$dimensionality == "space" ) {

    #nothing else to do

  } else if ( p$dimensionality == "space-time" ) {

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)
    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = discretize_data( span=c(0, 1, p$nw), toreturn="lower" )  # left breaks .. (c(1:p$nw)-1) / p$nw # intervals of decimal years... fractional year breaks
    p$cyclic_levels = factor(discretize_data( span=c(0, 1, p$nw), toreturn="midpoints" ), ordered=TRUE )  # though not used for prediction, can be used for modelling
    if (!exists("prediction_dyear", p)) p$prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()
    if (!exists("nt", p)) p$nt = p$ny   # ie, default is an annual model (no p$nw)
    if (!exists("prediction_ts", p)) p$prediction_ts = p$yrs + p$prediction_dyear # output timeslices for predictions in decimla years, yes all of them here

  } else if ( p$dimensionality == "space-time-cyclic" ) {

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)
    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = discretize_data( span=c(0, 1, p$nw), toreturn="lower" )  # left breaks .. (c(1:p$nw)-1) / p$nw # intervals of decimal years... fractional year breaks
    p$cyclic_levels = factor( discretize_data( span=c(0, 1, p$nw), toreturn="midpoints" ), ordered=TRUE )
    if (!exists("prediction_dyear", p)) p$prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()
    if (!exists("nt", p)) p$nt = p$nw*p$ny # i.e., seasonal with p$nw (default is annual: nt=ny)
    if (!exists("prediction_ts", p)) {
      # predictions at these time values (decimal-year), # output timeslices for predictions in decimla years, yes all of them here
      tout = expand.grid( yr=p$yrs, dyear=discretize_data( span=c(0, 1, p$nw), toreturn="midpoints" ), KEEP.OUT.ATTRS=FALSE )
      p$prediction_ts = sort( tout$yr + tout$dyear  ) # mid-points
    }
  }

  return(p)
}
