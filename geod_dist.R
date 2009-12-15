geod.dist <- function(lon1, lat1, lon2, lat2)
#
# Calculate geodesic distance (in m) between two points specified by 
# latitude/longitude (in numeric degrees) using Vincenty inverse
# formula for ellipsoids
#
# JavaScript code (c) 2002-2006 Chris Veness, 
#	GNU LGPL http://www.gnu.org/licenses/lgpl.html
#	http://www.movable-type.co.uk/scripts/latlong-vincenty.html
# R translation (c) 2006-2009 Jean-Olivier Irisson
#   GNU General Public License
#
{
	# Allow some of the arguments to be vectors
    # will fail if dimensions are incompatible
	d <- data.frame(lon1=lon1, lat1=lat1, lon2=lon2, lat2=lat2)
    d$out <- NA

    for (i in 1:nrow(d)) {
        # TODO Resorting on a loop is slow. Try to vectorize this
        lon1 <- d$lon1[i]
        lat1 <- d$lat1[i]
        lon2 <- d$lon2[i]
        lat2 <- d$lat2[i]
        
        # WGS-84 ellipsiod
    	a <- 6378137
    	b <- 6356752.3142
    	f <- 1/298.257223563

    	rad = function (x) {(x * pi)/180}
    	L <- rad(lon2-lon1)
    	U1 <- atan((1-f) * tan(rad(lat1)))
    	U2 <- atan((1-f) * tan(rad(lat2)))
    	sinU1 <- sin(U1)
    	cosU1 <- cos(U1)
    	sinU2 <- sin(U2)
    	cosU2 <- cos(U2)

    	lambda <- L 
    	lambdaP <- 2*pi
    	iterLimit <- 20
    	out <- NULL

    	while (abs(lambda-lambdaP) > 1e-12 & iterLimit>0) {
    		sinLambda <- sin(lambda) 
    		cosLambda <- cos(lambda)
    		sinSigma <- sqrt((cosU2*sinLambda) * (cosU2*sinLambda) + (cosU1*sinU2-sinU1*cosU2*cosLambda) * (cosU1*sinU2-sinU1*cosU2*cosLambda))
    		if (sinSigma==0) {
    			out <- 0	# co-incident points
    			break
    		}
    		cosSigma <- sinU1*sinU2 + cosU1*cosU2*cosLambda
    		sigma <- atan2(sinSigma, cosSigma)
    		sinAlpha <- cosU1 * cosU2 * sinLambda / sinSigma
    		cosSqAlpha <- 1 - sinAlpha*sinAlpha
    		cos2SigmaM <- cosSigma - 2*sinU1*sinU2/cosSqAlpha
    		if (is.infinite(cos2SigmaM)) {
    			cos2SigmaM <- 0	# equatorial line cosSqAlpha=0
    		}
    		C <- f/16*cosSqAlpha*(4+f*(4-3*cosSqAlpha))
    		lambdaP <- lambda
    		lambda <- L + (1-C) * f * sinAlpha *	(sigma + C*sinSigma*(cos2SigmaM+C*cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)))
    		iterLimit <- iterLimit-1
    	}
    	if (iterLimit==0) {
    		warning("Vincenty distance computation failed to converge")
    		out <- NA	# formula failed to converge
    	}
    	# if out is still defined as NULL, continue the computation
    	if (is.null(out)) {
    		uSq <- cosSqAlpha * (a*a - b*b) / (b*b)
    		A <- 1 + uSq/16384*(4096+uSq*(-768+uSq*(320-175*uSq)))
    		B <- uSq/1024 * (256+uSq*(-128+uSq*(74-47*uSq)))
    		deltaSigma <- B*sinSigma*(cos2SigmaM+B/4*(cosSigma*(-1+2*cos2SigmaM*cos2SigmaM)- B/6*cos2SigmaM*(-3+4*sinSigma*sinSigma)*(-3+4*cos2SigmaM*cos2SigmaM)))
    		s <- b*A*(sigma-deltaSigma)
    		out <- s
    	}
    	
    	d$out[i] <- out
    }

	return(d$out)
}
