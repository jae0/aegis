##################################################################
#
#
#                              >>> Geodesy In R <<<
#
#    This file contains a series of geodetic functions to aid in performing
#    spatial analysis across large distances (100s of km). Note: R Trig.
#    functions take arguments in radians. 1 degree = pi/180 radians
#
# original source by Darren Gillis and others
#
#	Various Chart Datum Parameters:
#
#	Name           Major Axis, a (km)   Flattening (f)
#	WGS84          6378.13700	          1/298.257223563
#	GRS80/NAD83    6378.13700	          1/298.257222101
#	WGS66          6378.145             1/298.25
#	GRS67/IAU68    6378.16000           1/298.2472
#	WGS72          6378.135             1/298.26
#	Krasovsky      6378.245             1/298.3
#	Clarke66/NAD27 6378.2064            1/294.9786982138
#
#	Coordinate Systems and Map Projections, D. H. Maling (Pergamon 1992)
#	Summarized by Ed Williams (http://williams.best.vwh.net/index.html)
#
#     Earth Radius as geometric mean of major & minor radia (WGS84)
#     Nautical Miles per degree as 57.29
#
#
#	Note: Radius of minor axis = a - a*f
#
# test data
#      point     = data.frame(lat=-30, lon=150)
#      locations = data.frame(lat=c(-31, -35, -40), lon=c(150, 151, 151))
#      vincenty distances = 110.8609, 562.3759, 1113.1415
#      great circle =       111.1327  563.5021  1115.0260
##################################################################




##################################################################
# vincenty method .. more accurate
# the original fortran code and documentation is copied at the bottom 
##################################################################




vincenty = function (loc1, loc2, a, f) {

  ## for history and equations:: see https://github.com/OSGeo/proj.4/wiki/GeodesicCalculations
  warning( "Deprecated. Consider using geosphere::distVincetyEllipsoid " )

  names(loc1) = c("lon", "lat")
  names(loc2) = c("lon", "lat")
  
      eps = 0.5e-13
      pi = 3.1415926535897932384626433832795
      rad = 0.0174532925199432957692369076848861
      r = 1.0-f

#     convert decimal degrees to radians

      rloc1 = rad*loc1
      rloc2 = rad*loc2
      
      tu1 = r * sin(rloc1$lat) / cos(rloc1$lat)
      tu2 = r * sin(rloc2$lat) / cos(rloc2$lat)
     
      cu1 = 1. / sqrt(tu1 * tu1 + 1. )
      su1 = cu1 * tu1
      cu2 = 1. / sqrt(tu2 * tu2 + 1. )
      
      s =   cu1 * cu2
      baz = s * tu2
      faz = baz * tu1
      dlon = (rloc2$lon - rloc1$lon) 
      x=dlon
      
      q = c(1:dim(loc2)[1]) # get it started into the loop

      sx = NULL
      cx = NULL
      tu1 = NULL
      tu2 = NULL
      sy = NULL
      cy = NULL
      y = NULL
      sa = NULL
      c2a = NULL
      cz = NULL
      e = NULL
      c = NULL

      
  		repeat {
  			sx[q] = sin(x[q])
        cx[q] = cos(x[q])
        tu1[q] = cu2[q] * sx[q]
        tu2[q] = baz[q] - su1 * cu2[q] * cx[q]
        sy[q] = sqrt(tu1[q] * tu1[q] + tu2[q] * tu2[q])
        cy[q] = s[q] * cx[q] + faz[q]
        y[q] = atan2(sy[q], cy[q])
        sa[q] = s[q] * sx[q] / sy[q]
        c2a[q] = - sa[q] * sa[q] + 1.
        nz = intersect(which(c2a != 0), q)
        cz[q] = faz[q] + faz[q]
        cz[nz] = -cz[nz] / c2a[nz] + cy[nz]
        e[q] = cz[q] * cz[q] * 2 - 1
        c[q] = ((-3. * c2a[q] + 4.) * f + 4.) * c2a[q] * f / 16.
        d = x
        x[q] = ((e[q] * cy[q] * c[q] + cz[q]) * sy[q] * c[q] + y[q]) * sa[q]
        x[q] = (1. - c[q]) * x[q] * f + dlon[q]
        q = which(abs(d - x) > eps)
        if (length(q) < 1) break
      }

      #faz = atan2(tu1, tu2)
      #baz = atan2(cu1 * sx, baz * cx - su1 * cu2) + pi
      x = sqrt( (1. / r / r - 1.) * c2a + 1. ) + 1.
      x = (x - 2.) / x
      c = 1. - x
      c = (x * x / 4. + 1.) / c
      d = (0.375 * x * x - 1.) * x
      x = e * cy
      s = 1. - e - e
      s = ((((sy * sy * 4. - 3.) * s * cz * d / 6. - x ) * d / 4. + cz) * sy * d + y) * c * a * r
      
      s [!is.finite(s)] = 0
      
      #faz = faz / rad # these are azimuths .. not needed
      #baz = baz / rad

      return(s)

      }


##################################################################
#~ C *** SOLUTION OF THE GEODETIC INVERSE PROBLEM AFTER T.VINCENTY
#~ C *** MODIFIED RAINSFORD'S METHOD WITH HELMERT'S ELLIPTICAL TERMS
#~ C *** EFFECTIVE IN ANY AZIMUTH AND AT ANY DISTANCE SHORT OF ANTIPODAL
#~ C *** STANDPOINT/FOREPOINT MUST NOT BE THE GEOGRAPHIC POLE
#~ C
#~ C *** A IS THE SEMI-MAJOR AXIS OF THE REFERENCE ELLIPSOID
#~ C *** F IS THE FLATTENING (NOT RECIPROCAL) OF THE REFERNECE ELLIPSOID
#~ C *** LATITUDES AND LONGITUDES IN RADIANS POSITIVE NORTH AND EAST
#~ C *** FORWARD AZIMUTHS AT BOTH POINTS RETURNED IN RADIANS FROM NORTH
#~ C
#~ C *** PROGRAMMED FOR CDC-6600 BY LCDR L.PFEIFER NGS ROCKVILLE MD 18FEB75
#~ C *** MODIFIED FOR IBM SYSTEM 360 BY JOHN G GERGEN NGS ROCKVILLE MD 7507
#~ C
#~ C *** Modified for R by D.Gillis Zoology University of Manitoba 16JUN03
#~ C     Replaced common blocks for constants with DATA statements.  Datum
#~ C     parameters moved from common block to subroutine arguements.
#~ C
#~ C *** Input Variables: DLAT1,DLON1 - initial fix (P1) in degrees
#~ C                          (latitude, north +) (longitude east +)
#~ C                      DLAT2,DLON2 - destination fix (P2) in degrees
#~ C
#~ C                      Ellipsoid (spheroid model, eg. WGS84)
#~ C                         A   - radius of major axis in distance units
#~ C                         F   - flattening factor
#~ C
#~ C *** Output Variables: FAZ - azimuth of the geodesic (P1 to P2)
#~ C                       BAZ - azimuth of the geodesic (P2 to P1)
#~ C                       S   - spheroidal distance = length of the geodesic
#~ C                             in distance units
#~ C
#~ C *** After Vincenty,T. 1975. Direct and inverse solutions of geodesics
#~ C           on the ellipsoid with application of nested equations. Survey
#~ C           Review 23(176):88-94.
#~ C
#~ C
#~ C     These routines were compiled in Windows 98 (DOS Window) using the
#~ C     Gnu FORTRAN complier: g77 --share -o geodesy.dll marspat.for
#~ C     creating:             geodesy.dll
#~ C
#~ C
#~ C
#
#  original code ("geodesy.for") is as follows:
#     note the return of azimuths has been turned off
#
      #~ SUBROUTINE INVER1(DLAT1,DLON1,DLAT2,DLON2,A,F,FAZ,BAZ,S)
      #~ IMPLICIT DOUBLE PRECISION (A-H,O-Z)
#~ C     COMMON/CONST/PI,RAD - original code
#~ C     COMMON/ELIPSOID/A,F - original code
      #~ DATA EPS/0.5D-13/
      #~ DATA PI/3.1415926535897932384626433832795D0/
      #~ DATA RAD/0.0174532925199432957692369076848861D0/
      #~ R=1.D0-F
#~ C
#~ C     Convert decimal degrees to radians
#~ C
      #~ GLAT1=DLAT1*RAD
      #~ GLON1=DLON1*RAD
      #~ GLAT2=DLAT2*RAD
      #~ GLON2=DLON2*RAD
#~ C
      #~ TU1=R*DSIN(GLAT1)/DCOS(GLAT1)
      #~ TU2=R*DSIN(GLAT2)/DCOS(GLAT2)
      #~ CU1=1./DSQRT(TU1*TU1+1.)
      #~ SU1=CU1*TU1
      #~ CU2=1./DSQRT(TU2*TU2+1.)
      #~ S=CU1*CU2
      #~ BAZ=S*TU2
      #~ FAZ=BAZ*TU1
      #~ X=GLON2-GLON1
  #~ 100 SX=DSIN(X)
      #~ CX=DCOS(X)
      #~ TU1=CU2*SX
      #~ TU2=BAZ-SU1*CU2*CX
      #~ SY=DSQRT(TU1*TU1+TU2*TU2)
      #~ CY=S*CX+FAZ
      #~ Y=DATAN2(SY,CY)
      #~ SA=S*SX/SY
      #~ C2A=-SA*SA+1.
      #~ CZ=FAZ+FAZ
      #~ IF(C2A.GT.0.)CZ=-CZ/C2A+CY
      #~ E=CZ*CZ*2.-1.
      #~ C=((-3.*C2A+4.)*F+4.)*C2A*F/16.
      #~ D=X
      #~ X=((E*CY*C+CZ)*SY*C+Y)*SA
      #~ X=(1.-C)*X*F+GLON2-GLON1
      #~ IF(DABS(D-X).GT.EPS) GOTO 100
      #~ FAZ=DATAN2(TU1,TU2)
      #~ BAZ=DATAN2(CU1*SX,BAZ*CX-SU1*CU2)+PI
      #~ X=DSQRT((1./R/R-1.)*C2A+1.)+1.
      #~ X=(X-2.)/X
      #~ C=1.-X
      #~ C=(X*X/4.+1.)/C
      #~ D=(0.375*X*X-1.)*X
      #~ X=E*CY
      #~ S=1.-E-E
      #~ S=((((SY*SY*4.-3.)*S*CZ*D/6.-X)*D/4.+CZ)*SY*D+Y)*C*A*R
      #~ FAZ=FAZ/RAD
      #~ BAZ=BAZ/RAD
      #~ END
#
# example method of access to shared library from R
#
#~ dyn.load("~/src/grids/geodesy.lib")
	#~ inver1<- function (Lat1, Lon1, Lat2, Lon2) {
	              #~ a<-6378137.00          # WGS84 major axis
	              #~ f<-1/298.257223563     # WGS84 flattening parameter
                    #~ .Fortran("inver1",as.double(Lat1),as.double(Lon1),
                                      #~ as.double(Lat2),as.double(Lon2),
                                      #~ as.double(a),as.double(f),
                                      #~ as.double(0),as.double(0),
                                      #~ as.double(0)) [[9]]
      #~ }


  #~ GDvincenty <- function (Lat1, Lon1, Lat2, Lon2) {
 		#~ n = length(Lat1)
	  #~ ans = inver1(Lat1,Lon1,Lat2,Lon2)

    #~ if (n>1) {
      #~ for (i in 2:n) {
      	#~ ans<-c(ans, inver1(Lat1[i],Lon1[i],Lat2[i],Lon2[i]))
      #~ }
    #~ }
    #~ ans
	#~ }

#
##################################################################

