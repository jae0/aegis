
temporal_parameters = function( p=NULL, ... ) {

  p = parameters_control(p, list(...), control="add") # add passed args to parameter list, priority to args

  # ----------------------------------
  # determine problem's temporal dimensionality

  if ( p$aegis_dimensionality == "space" ) {

    #nothing else to do

  } else if ( p$aegis_dimensionality == "space-year" ) {

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)
    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = (c(1:p$nw)-1) / p$nw # intervals of decimal years... fractional year breaks
    p$dyear_centre = p$dyears[ floor(p$nw/2) ] + p$tres/2
    if (!exists("prediction_dyear", p)) p$prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()
    if (!exists("nt", p)) p$nt = p$ny   # ie, default is an annual model (no p$nw)
    if (!exists("prediction_ts", p)) p$prediction_ts = p$yrs + p$prediction_dyear # output timeslices for predictions in decimla years, yes all of them here

  } else if ( p$aegis_dimensionality == "space-year-season" ) {

    if (!exists("yrs", p)) p$yrs = c(1999:lubridate::year(lubridate::now()))  # years for modelling and interpolation
    p$ny = length(p$yrs)
    if (!exists("nw", p)) p$nw = 10 # default value of 10 time steps number of intervals in time within a year for all temp and indicators
    p$tres = 1/ p$nw # time resolution .. predictions are made with models that use seasonal components
    p$dyears = (c(1:p$nw)-1) / p$nw # intervals of decimal years... fractional year breaks
    p$dyear_centre = p$dyears[ floor(p$nw/2) ] + p$tres/2
    if (!exists("prediction_dyear", p)) p$prediction_dyear = lubridate::decimal_date( lubridate::ymd("0000/Sep/01")) # used for creating timeslices and predictions  .. needs to match the values in aegis_parameters()
    if (!exists("nt", p)) p$nt = p$nw*p$ny # i.e., seasonal with p$nw (default is annual: nt=ny)
    if (!exists("prediction_ts", p)) {
      # predictions at these time values (decimal-year), # output timeslices for predictions in decimla years, yes all of them here
      tout = expand.grid( yr=p$yrs, dyear=1:p$nw, KEEP.OUT.ATTRS=FALSE )
      p$prediction_ts = sort( tout$yr + tout$dyear/p$nw - p$tres/2 )# mid-points
    }
  }

  return(p)
}
