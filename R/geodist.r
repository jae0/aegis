  geodist = function (point, locations, method) {
		
    warning( "Deprecated. Consider using library geosphere::distVincentyEllipsoid(accurate), distMeeus(fast and accurate), distCosine, distGeo (most accurate) etc..")

    a = 6378.13700          # WGS84 major axis in km
	  f = 1/298.257223563   # the WGS84 flattening parameter .. do not simplify as round-off errors are important
    geometric.mean.radius =  sqrt(6378.13700*6356.75231)
   

    if (method == "vincenty") out = vincenty(point, locations, a, f)
	  if (method == "great.circle") out = great.circle.distance(point, locations, R=geometric.mean.radius)  
        # great.circle method can handle point-vector and vector-vector data
    return(out)
	}

  geodist.test = function (point, locations, method, threshold, type="lt") {
    d = geodist( point=point, locations=locations, method=method )
    if (type=="lt") i = which( d < threshold )
    if (type=="le") i = which( d <= threshold )
    if (type=="gt") i = which( d > threshold )
    if (type=="ge") i = which( d >= threshold )
    return(i)
	}

