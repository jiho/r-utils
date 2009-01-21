#
#	Useful functions to manipulate data (for plots in particular)
#	- sub sample
#	- interpolate
#	- reorganize
#	- etc.
#
#	(c) 2009 Jean-Olivier Irisson <irisson@normalesup.org>. 
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------


# Sub-sampling or interpolation
#------------------------------------------------------------

regrid <- function(x, shift=T)
#
#	Resample the same values on a grid twice as coarse
#
#	x		data.frame with components x and y
{
	if (shift) {
		x = x[x$x!=max(x$x) & x$y!=max(x$y),]
	}
	x = x[seq(1,nrow(x),by=2),]
	return(x)
}

interp.x <- function(x, y=NULL, n=80, xo=seq(min(x),max(x),length=n), method="spline", ...)
#
#	Interpolate data y defined at points x to points xo 
#	x can also be a data.frame with x in the first column and y in the second
#	method can be "spline" or "linear"
#
{
	method = match.arg(method,c("spline","linear"))
	if (is.data.frame(x)) {
		y = x[,2]
		x = x[,1]
	}
	
	if (method == "spline") {
		fun = splinefun(x,y)
		yo = fun(xo)
	} else if (method == "linear") {
		yo = approx(x,y,xout=xo)$y
	}

	return(data.frame(x=xo,y=yo))
}

interp.xy <- function(x, y, z, n=80, xo=seq(min(x),max(x),length=n), yo=seq(min(y),max(y),length=n), extrapolate=F, method="akima", output="list")
#
#	Interpolates data z defined at points (x,y) on a new grid
#
#	x, y				coordinated of input points
#	z					values at input points
#	n					number of point in the new grid
#	xo, yo			coordinates of output points
#	extrapolate		if T, also define points outside the range of x,y when possible
#	method
#		"akima"			bivariate smooth interpolation (package akima)
#		"krigging"		kriging	(package fields) -- not yet implemented
#		"bilinear"		fast bilinear (package fields)
#	output
#		"data.frame"	data.frame with columns x, y and z (for ggplot)
#		"list,matrix"	list with components x, y, and z (for persp, contour)
#
{
	require("reshape")

	# parse arguments
	method = match.arg(method,c("akima","bilinear"))
	output = match.arg(output,c("data.frame","list","matrix"))

	if (method=="akima") {
		# interpolate a regular grid from a set of irregular points
		require("akima")
		out = interp(x, y, z, xo, yo, linear=F, extrap=extrapolate)
		
		if (output == "data.frame") {
			out = list2frame(out)
		}
	
	} else if (method == "bilinear") {
		# interpolate a regular grid from a set of gridded points
		library("fields")
		
		# original coordinates
		objDat = data.frame(x=x,y=y,value=z)
		obj = list(x=sort(unique(x)),y=sort(unique(y)))
		obj$z = as.matrix(cast(objDat,x~y))
		
		# interpolated locations
		locs = make.surface.grid( list(xo, yo) )
		
		out = interp.surface(obj, locs)
		out = data.frame(x=locs[,1], y=locs[,2], z=out)
		
		if (output %in% c("list","matrix")) {
			out=frame2list(out)
		}
	}
	
	return(out)
}


# Data re-organisation
#------------------------------------------------------------

frame2list <- function(X)
#
#	Turn a data.frame with columns x, y (or lon, lat) and a value, into a list suitable for persp, contour and the like
#
#	X		data.frame with components x, y (or lon, lat) and one other
{
	require("reshape")

	if (!is.data.frame(X)) {
		stop("Need a data.frame")
	}

	# reorder columns
	if ( all(c("x","y") %in% names(X)) ) {
		X = X[,c("x","y",setdiff(names(X),c("x","y")))]
	} else if ( all(c("lon","lat") %in% names(X)) ) {
		X = X[,c("lon","lat",setdiff(names(X),c("lon","lat")))]
	}
	
	# renames them because its easier
	names(X) = c("x","y","value")
	
	# create output list
	out = list(x=sort(unique(X$x)),y=sort(unique(X$y)))
	out$z = as.matrix(cast(X,x~y))

	return(out)
}

list2frame <- function(X)
#
#	Turn a list with components x, y, z (suitable for persp and the like) into a data.frame with columns x, y, z, suitable for ggplot
#
#	X		list with components x, y, and z
{
	require("reshape")
	
	if (!is.list(X)) {
		stop("Need a list")
	}

	if ( !all(names(X) %in% c("x","y","z")) ) {
		stop("Need a list with 3 components named x, y and z")
	}
	
	out = melt(X$z,varnames=c("x","y"))
	out$x = X$x[out$x]
	out$y = X$y[out$y]
	out = rename(out,c(value="z"))

	return(out)
}

oust <- function(x, ...)
#
#	Remove an elements whose names are in ... from a list/data.frame
#	(useful to pull out elements of a ggplot)
#
#	x		list or data.frame
{
	oldClass = class(x)
	# convert ... to a vector of characters
	# oust(x, foo, bar) => element=c("foo","bar")
	element = as.character(match.call()[-1])[-1]
	# expand element names so that they can be abbreviated
	element = match.arg(element, names(x), several.ok=T)
	x = x[setdiff(names(x),element)]
	class(x) = oldClass
	return(x)
}

tsou <- function(x, ...)
#
#	Remove an elements whose names are NOT in ... from a list/data.frame
#	(usefull to pull out elements of a ggplot)
#
#	x		list or data.frame
{
	oldClass = class(x)
	# convert ... to a vector of characters
	# oust(x, foo, bar) => element=c("foo","bar")
	element = as.character(match.call()[-1])[-1]
	# expand element names so that they can be abbreviated
	element = match.arg(element, names(x), several.ok=T)
	x = x[intersect(names(x),element)]
	class(x) = oldClass
	return(x)
}

