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


outliers <- function(x, method="hampel", factor=5.2)
#
#	Return the indices of outliers in x, according to:
#		. Davies and Gather, The identification of multiple outliers, JASA 88 (1993), 782-801. for methods hampel, g and custom
#		. outlier.test in package car for method bonferroni
#	In method custom, the higher the factor the less sensible the detection of outliers
#
{
	method = match.arg(method,c("hampel","g","bonferroni","custom"))

	if (method=="bonferroni") {
		suppressPackageStartupMessages(require("car"))
		return(as.numeric(outlier.test(lm(x~1))$obs))
	} else {
		n = length(x)
		if (method=="hampel") {
			factor = 5.2
		} else if (method=="g") {
			if (n%%2==0) {
				factor = 2.906+11.99*(n-6)^-0.5651
			} else {
				factor = 2.906+12.99*(n-5)^-0.5781
			}
		} else if (method=="custom") {
			factor = factor
		}
		return(which(abs(x-median(x))>(factor*mad(x))))
	}
}


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

interp.xy <- function(x, y, z, n=80, xo=seq(min(x),max(x),length=n), yo=seq(min(y),max(y),length=n), extrapolate=F, method=c("akima", "bilinear"), output=c("list","data.frame"))
#
#	Interpolates data z defined at points (x,y) on a new grid
#
#	x, y                coordinated of input points
#	z                   values at input points
#	n                   number of point in the new grid
#	xo, yo              coordinates of output points
#	extrapolate         if T, also define points outside the range of x,y when possible
#	method
#	    "akima"         spline interpolation (package akima)
#	    "bilinear"      simple bilinear
#	output
#	    "data.frame"    data.frame with columns x, y and z (for ggplot)
#	    "list,matrix"   list with components x, y, and z (for persp, contour)
#
{
	suppressPackageStartupMessages(require("reshape"))

	# parse arguments
	method = match.arg(method)
	output = match.arg(output)

	if (method=="akima") {
		# interpolate a regular grid from a set of irregular points
		suppressPackageStartupMessages(require("akima"))
		out = interp(x, y, z, xo, yo, linear=F, extrap=extrapolate)

		if (output == "data.frame") {
			out = list2frame(out)
		}

	} else if (method == "bilinear") {
		# interpolate a regular grid from a set of gridded points

		# original coordinates
		objDat <- data.frame(x=x, y=y, value=z)
		x <- sort(unique(x))
		y <- sort(unique(y))
		z <- as.matrix(cast(objDat,x~y))

		# interpolated locations
		locs <- expand.grid(xo, yo)
		xNew <- locs[, 1]
		yNew <- locs[, 2]

		# find indexes of cells in the original grid that contain the points to be interpolated
		nx <- length(x)
		ny <- length(y)
		lx <- approx(x,1:nx,xNew)$y
		ly <- approx(y,1:ny,yNew)$y
		lx1 <- floor(lx)
		ly1 <- floor(ly)

		# distance between grid cells origins and points
		ex <- lx - lx1
		ey <- ly - ly1

		# for points that are exactly on the top or right of the grid, shift one cell down (cf formula below where 1 is added to the index)
		ex[lx1 == nx] <- 1
		ey[ly1 == ny] <- 1
		lx1[lx1 == nx] <- nx - 1
		ly1[ly1 == ny] <- ny - 1
				
		# bilinear interpolation
		out <- z[cbind(lx1  , ly1  )] * (1 - ex) * (1 - ey) + 
		       z[cbind(lx1+1, ly1  )] * ex       * (1 - ey) +
		       z[cbind(lx1  , ly1+1)] * (1 - ex) * ey + 
		       z[cbind(lx1+1, ly1+1)] * ex       * ey
		out <- data.frame(x=xNew, y=yNew, z=out)

		if (output == "list") {
			out = frame2list(out)
		}
	}

	return(out)
}


# Data re-organisation
#------------------------------------------------------------

frame2list <- function(X, names=c("x","y","value"))
#
#	Turn a data.frame with two coordinates columns and a value, into a list suitable for persp, contour and the like
#
#	X		data.frame with components x, y (or lon, lat) and a value
{
	if (!is.data.frame(X)) {
		stop("Need a data.frame")
	}

	if (all(names %in% names(X))) {
		# if all names are in the data frame, extract the columns
		X <- X[,names]
	} else if (ncol(X) == 3) {
		# if the data.frame names do not match but it has the right size, assume columns are in order and rename them
		warning("Assuming columns ", paste(names(X), collapse=","), " are in fact ", paste(names, collapse=","))
		names(X) <- names
	} else {
		stop("Cannot find coordinates and values in this data.frame. Check column names")
	}

	# convert into list
	suppressPackageStartupMessages(require("reshape"))
	out = list(x=sort(unique(X$x)),y=sort(unique(X$y)))
	out$z = as.matrix(cast(X,x~y))

	return(out)
}

list2frame <- function(X, names=c("x", "y", "z"))
#
#	Turn a list with three components (suitable for persp and the like) into a data.frame with columns x, y, z, suitable for ggplot
#
#	X		list with components x, y, and z
{
	if (!is.list(X)) {
		stop("Need a list")
	}

	if ( is.null(names(X)) & length(X) == 3 ) {
		# if the list has no names and the right size, assume components are in order and rename them
		warning("Assuming list components are in the order: ", paste(names, collapse=","))
		names(X) <- names
	} else if ( !all(names %in% names(X)) & length(X) == 3 ) {
		# if the list names do not match but it has the right size, assume components are in order and rename them
		warning("Assuming components ", paste(names(X), collapse=","), " are in fact ", paste(names, collapse=","))
		names(X) <- names
	} else if (all(names %in% names(X))) {
		# if all names are in the list, extract the corresponding components
		X <- X[names]
	} else {
		stop("Cannot find coordinates and values in this list. Check names")
	}

	# convert into data.frame
	suppressPackageStartupMessages(require("reshape"))
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
	# convert all arguments in ... to a vector of characters:
	# oust(x, foo, bar) => element=c("foo","bar")
	element = as.character(match.call()[-1])[-1]
	# if ... happens to contain only one element which is a vector of names, use them
	if (length(element)==1) {
		if (exists(element)) {
			element = as.character(get(element))
		}
	}
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
#	...	vector of names or unquotted names of data.frame columns
{
	oldClass = class(x)
	# convert all arguments in ... to a vector of characters:
	# oust(x, foo, bar) => element=c("foo","bar")
	element = as.character(match.call()[-1])[-1]
	# if ... happens to contain only one element which is a vector of names, use them
	if (length(element)==1) {
		if (exists(element)) {
			element = as.character(get(element))
		}
	}
	# expand element names so that they can be abbreviated
	element = match.arg(element, names(x), several.ok=T)
	x = x[intersect(names(x),element)]
	class(x) = oldClass
	return(x)
}

extract <- function(x, ..., drop=FALSE)
#
#	Restricts information in x to the lines matching the conditions in ...
# 	The extraction criteria take the form: x_variable name = values extracted
#		e.g. family = c("pomacentridae","acanthuridae")
#	Both the names of the variables and the names of the values can be abbreviated as long as the abbreviation is unambiguous
#		e.g. fam = c("pomacent","acanth")
#	NA or NULL values extract everything (i.e. are equivalent to not specifying the condition at all)
#
{
	# get selection variables
	subsets = list(...)
	# if there are no extraction arguments, just return the input
	if (length(subsets) == 0) {
		return(x)
	}
	# allow the selection variables to be abbreviated
	names(subsets) = match.arg(names(subsets), names(x), several.ok=TRUE)
	if (any(is.na(names(subsets)))) {
		stop(paste("Ambiguity on extraction variables. Identify one in:", paste(names(x), collapse=" "), sep="\n"))
	}

	# for those which are NAs or NULL, just fill them with all possibilites
	# for those which are character variables, allow them to be abbreviated
	alls = lapply(x[names(subsets)], unique)
	for (i in 1:length(subsets)) {
		if (all(is.na(subsets[[i]])) || is.null(subsets[[i]])) {
			subsets[[i]] = alls[[i]]
		} else if (is.character(alls[[i]]) | is.factor(alls[[i]])) {
			subsets[[i]] = match.arg(as.character(subsets[[i]]), alls[[i]], several.ok=TRUE)
		}
	}

	# compute all possibilities
	allSubsets = expand.grid(subsets)

	# extract only those in common between x and those possibilities
	out = sorted.merge(x, allSubsets)

	# reorder output columns to match input
	out = out[names(x)]

	# recompute factor levels if necessary
	if (drop) {
		cout = out[names(subsets)]
		factCols = sapply(cout, is.factor)
		cout[factCols] = lapply(cout[factCols],factor)
		out[names(subsets)] = cout
	}

	return(out)
}

sorted.merge <- function(x, y, ...)
#
#	Merges both arguments with merge and keeps the order in x
#
{
	m <- merge(cbind(id=seq_len(nrow(x)), x), y, ...)
	m <- m[order(m$id), !names(m)%in%"id"]
	return(m)
}

closest.index <- function(x,y)
#
#	Find the indexes of x such as the corresponding elements are closest to the values in y
#
{
	round(approx(x,1:length(x),y)$y)
}
