#
#	Access to and plots of NetCDF files
#		with particular application to OPSIME output
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------


# Extract NetCDF variables
#------------------------------------------------------------

nc.slice <- function(nc, varname=NA, ...)
#
#	Cut a 2D slice of a netCDF variable
#
#	nc			netCDF handle
#	varname	variable name
#	...		indexes to extract
#					either named vectors of indexes, in which case the unspecified elements are fully extracted
#					or unnamed vectors, in which case dimensions are assumed to be in the same order that in the netCDF record and NA elements are fully extracted
#
{
	extract=list(...)

	# Fetch variable name
	varname = match.arg(varname, names(nc$var))

	# Get info for this variable
	cVar = nc$var[[varname]]		# current variable
	cDims = nc$dim[cVar$dimids]	# current dimensions
	cDimNames = sapply(cDims,function(x){x$name})	# current dimension names

	# Check whether variable is extracted based on dimension names or list of all dimensions
	extractNames = names(extract)

	if (!is.null(extractNames)) {
		# 1. Use the names

		# detect matching names
		matching <- pmatch(cDimNames, extractNames, duplicates.ok=TRUE)

        # keep only dimensions that match
		cExtract <- extract[matching]

        # dimensions that do not match are NULL, replace that by NA
		cExtract[is.na(matching)] <- NA

	} else {
		# 2. Consider that the list contains all the dimensions in the correct order or is empty entirely
		if (length(extract) == 0 ) {
			# If the list is empty, just put NAs everywhere: we extract everything
			cExtract = vector("list",length(cDims))
			cExtract[] = NA
		} else if (length(extract) != length(cDims)) {
			# Safety check
			stop("Not the right number of dimensions in extract")
		} else {
			# Consider that extract contains all dimensions in correct order
			cExtract = extract
		}
	}
	names(cExtract) = cDimNames

	# For NA extraction vectors, just keep the full dimension
	# For the other ones, keep the indices
	for (i in 1:length(cDims)) {
		if (all(is.na(cExtract[[i]]))) {
			cDims[[i]]$indices = 1:cDims[[i]]$len
		} else {
			cDims[[i]]$indices = cExtract[[i]]
			cDims[[i]]$vals = cDims[[i]]$vals[cExtract[[i]]]
		}
	}

	# Compute start and count vectors
	indices = lapply(cDims,`[[`,"indices")
	start = sapply(indices,min)
	stop =  sapply(indices,max)
	count = stop-start+1

	# Extract slice
	slice = get.var.ncdf(nc, varname, start=start, count=count)
	cVar$vals = slice

	return(list(var=cVar,dims=cDims))
}

slice2data <- function(slice)
#
#	Melt 2D slice into a data.frame
#
{
	suppressPackageStartupMessages(require("reshape"))

	# Get plot coordinates : first two dimensions such that the index length is not 1
	isCoord = lapply(slice$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	# if (sum(isCoord) != 2) {
	# 	stop("Not a 2 D slice")
	# }
	coords = slice$dims[isCoord]

	# Melt data extracted from netCDF file
	var = melt(slice$var$vals,varnames=sapply(coords,`[[`,"name"))

	# fetch variable name
	var <- rename(var, c(value=slice$var$name))

	# fetch real coordinates values
	for (i in 1:length(coords)) {
		var[,i] = coords[[i]]$vals[var[,i]]
	}

	return(var)
}


# Plotting functions
#------------------------------------------------------------
f.contour.slice <- function(slice, min=NA, max=NA, nlevels=200, ...)
#
#	Filled contour plot of given slice [deprecated]
#
#	slice		2D slice as extracted by nc.slice
#	min/max	range of values to include in the plot
#	...		further arguments to filled.contour
#
{

	# Get plot coordinates : first two dimensions such that the index length is not 1
	isCoord = lapply(slice$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	if (sum(isCoord) != 2) {
		stop("Not a 2 D slice")
	}
	coords = slice$dims[isCoord]

	# Plot
	if (is.na(min)) {
		min = min(slice$var$vals)
	}
	if (is.na(max)) {
		max = max(slice$var$vals)
	}

	filled.contour(coords[[1]]$vals, coords[[2]]$vals, slice$var$vals, zlim=c(min,max), color=jet.colors, asp=1, nlevels=nlevels, ...)

	# Title
	title(main=slice$var$longname, sub=slice$var$units, xlab=paste(coords[[1]]$name, coords[[1]]$units),ylab=paste(coords[[2]]$name, coords[[2]]$units))

	# Legend for non coords variables
	extras = slice$dims[!isCoord]
	extrasLegend = paste(sapply(extras,`[`,c("name","vals")),collapse=": ")
	mtext(extrasLegend)
}

contour.slice <- function(slice, min=NA, max=NA, nlevels=10, ...)
#
#	Contour plot of given slice [deprecated]
#
#	slice		2D slice as extracted by nc.slice
#	min/max	range of values to include in the plot
#	...		further arguments to contour
#
{

	# Get plot coordinates : first two dimensions such that the index length is not 1
	isCoord = lapply(slice$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	if (sum(isCoord) != 2) {
		stop("Not a 2 D slice")
	}
	coords = slice$dims[isCoord]

	# Plot
	if (is.na(min)) {
		min = min(slice$var$vals)
	}
	if (is.na(max)) {
		max = max(slice$var$vals)
	}

	contour(coords[[1]]$vals, coords[[2]]$vals, slice$var$vals, zlim=c(min,max), asp=1, col = jet.colors(nlevels), nlevels=nlevels, ...)

	# Title
	title(main=slice$var$longname, sub=slice$var$units, xlab=paste(coords[[1]]$name, coords[[1]]$units),ylab=paste(coords[[2]]$name, coords[[2]]$units))

	# Legend for non coords variables
	extras = slice$dims[!isCoord]
	extrasLegend = paste(sapply(extras,`[`,c("name","vals")),collapse=": ")
	mtext(extrasLegend)
}

ggadd.tile <- function(slice, min=NA, max=NA)
#
#	Add a tile plot of the slice
#
#	slice		2D slice as extracted by nc.slice
#	min/max	range of values to include in the plot and legend
#
{
	suppressPackageStartupMessages(require("ggplot2"))

	# Get plot coordinates : first two dimensions such that the index length is not 1
	isCoord = lapply(slice$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	if (sum(isCoord) != 2) {
		stop("Not a 2 D slice")
	}
	coords = slice$dims[isCoord]

	# Define value range
	if (is.na(min)) {
		min = min(slice$var$vals)
	}
	if (is.na(max)) {
		max = max(slice$var$vals)
	}

	# Melt data extracted from netCDF file
	var = melt(slice$var$vals,varnames=sapply(coords,`[[`,"name"))
	names(var)[1:2] = c("x","y")
	# fetch real coordinates values
	var[,1] = coords[[1]]$vals[var[,1]]
	var[,2] = coords[[2]]$vals[var[,2]]
    # TODO Use names instead of juste 1,2 here

	# Tile
	g = geom_tile(data=var,mapping=aes(x=x,y=y,fill=value))

	# Axes labels
	g = c(g, scale_x_continuous(paste(coords[[1]]$name, coords[[1]]$units), expand=c(0,0)), scale_y_continuous(paste(coords[[2]]$name, coords[[2]]$units), expand=c(0,0)) )

	# Add nice color gradient
	g = c(g,scale_fill_gradient2(paste(slice$var$name, slice$var$units), limits=c(min,max), low="blue", mid="yellow", high="red"))

	# Add title/legend
	extras = slice$dims[!isCoord]
	extrasLegend = paste(sapply(extras,`[`,c("name","vals")),collapse=": ")
	title = paste(slice$var$longname,extrasLegend,sep=" ")

	return(list(g,opts(title=title)))
}

ggadd.field <- function(nc, uVariable="u", vVariable="v", sub=5, scale=1, ...)
#
#	Extract and plot U and V as an arrow field
#
#	nc		netCDF file handle
#	sub	number of grid point on which to subsample
#	...	arguments passed to nc.slice
#
{
	# Extract U and V
	sliceU = nc.slice(nc, uVariable, ...)
	sliceV = nc.slice(nc, vVariable, ...)

	# Get plot coordinates : first two dimensions such that the index length is not 1
	# Do it for U and not V but they should really be the same
	isCoord = lapply(sliceU$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	if (sum(isCoord) != 2) {
		stop("Not a 2 D slice")
	}
	coords = sliceU$dims[isCoord]
	coordsName = sapply(coords,`[[`,"name")

	# Convert them to data.frame form
	U = melt(sliceU$var$vals, varnames=coordsName)
	V = melt(sliceV$var$vals, varnames=coordsName)
	speeds = cbind(U[,1:2],u=U$value,v=V$value)

	# Subsample the grid
	ix = seq(1,max(speeds[,1]),by=sub)
	iy = seq(1,max(speeds[,2]),by=sub)
	speeds = speeds[speeds[,1]%in%ix & speeds[,2]%in%iy, ]

	# fetch real coordinates values
	speeds[,1] = coords[[1]]$vals[speeds[,1]]
	speeds[,2] = coords[[2]]$vals[speeds[,2]]

	# determine a scaling to make the arrows readable
	stepSizes = diff(unique(speeds[,1]))
	speeds$scale = stepSizes[1]*sub*scale

    # compute end points
    xName <- coordsName[1]
    yName <- coordsName[2]
	speeds$xEnd <- speeds[,xName]+scale*speeds$u
	speeds$yEnd <- speeds[,yName]+scale*speeds$v

    # put NAs where speed is 0
    speeds$xEnd[speeds$u==0 & speeds$v==0] <- NA
    # speeds$yEnd[speeds$u==0 & speeds$v==0] <- NA
    # NB: one NA is enough to not plot the segment

    # create arrow field
    g = geom_segment(data=speeds, aes_string(x=xName, y=yName, xend="xEnd", yend="yEnd"), arrow=arrow(length=unit(0.015,"npc"), angle=15))

	# Add title/legend
    varnames = paste(sliceU$var$longname,sliceV$var$longname,sep=" x ")
    extras = sliceU$dims[!isCoord]
    extrasLegend = paste(sapply(extras,`[`,c("name","vals")),collapse=": ")
    title = paste(varnames,extrasLegend,sep="\n")

    return(list(g, opts(title=title)))
}
