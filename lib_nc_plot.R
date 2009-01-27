#
#	Access to and plots of NetCDF files
#		with particular application to OPSIME output
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>. 
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------


# Access NetCDF files
#------------------------------------------------------------

nc.open <- function(filename="opsime.nc")
#
#	Opens netCDF file and compute useful indexes
{
	require("ncdf")

	# Opens the dataset and fetches its attributes
	nc = open.ncdf(filename)
	nc$att = nc.attr(nc)
	return(nc)
}

nc.attr <- function(nc)
#
#	Read global attributes of given file
{
	att = list()
	att$xStartin = att.get.ncdf(nc,0,att="xStartin")$value
	att$yStartin = att.get.ncdf(nc,0,att="yStartin")$value
	att$zStartin = att.get.ncdf(nc,0,att="zStartin")$value
	att$tStartin = att.get.ncdf(nc,0,att="tStartin")$value
	att$biolName = att.get.ncdf(nc,0,att="biolName")$value
	att$swimSpeedSettlement = att.get.ncdf(nc,0,att="swimSpeedSettlement")$value
	att$swimSpeedHatching = att.get.ncdf(nc,0,att="swimSpeedHatching")$value
	att$eggDuration  =att.get.ncdf(nc,0,att="eggDuration")$value
	att$preCompetencyDuration =att.get.ncdf(nc,0,att="preCompetencyDuration")$value
	att$maxLarvalDuration = att.get.ncdf(nc,0,att="maxLarvalDuration")$value
	att$swimEnduranceTimeSwum = att.get.ncdf(nc,0,att="swimEnduranceTimeSwum")$value
	att$swimEnduranceSpeedSwum = att.get.ncdf(nc,0,att="swimEnduranceSpeedSwum")$value
	return(att)
}

nc.close <- function(nc)
#
#	Close the netCDF file
{
	require("ncdf")
	close.ncdf(nc)
}

nc.reopen <- function(nc)
#
#	Close and reopen the nc file (useful when it is updated)
{
	nc.close(nc)
	ncNew = nc.open(nc$filename)
	return(ncNew)
}


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
		
		# Fetch correct names
		extractNames = match.arg(extractNames, cDimNames, several.ok=TRUE)
		# names(extract) = dimNames
		# TODO add a check for the number of non empty/non matched names
		
		# Create extract list with dimensions in correct order
		# Empty list with NAs
		cExtract = vector("list", length(cDims))
		cExtract[] = NA
		# Fill with the info we have
		idx = match(extractNames, cDimNames)
		cExtract[idx] = extract
		
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
	require("reshape")
	
	# Get plot coordinates : first two dimensions such that the index length is not 1
	isCoord = lapply(slice$dims,`[[`,"vals")
	isCoord = (sapply(isCoord,length)>1)
	# if (sum(isCoord) != 2) {
	# 	stop("Not a 2 D slice")
	# }
	coords = slice$dims[isCoord]
	
	# Melt data extracted from netCDF file
	var = melt(slice$var$vals,varnames=sapply(coords,`[[`,"name"))
	# fetch real coordinates values
	for (i in 1:length(coords)) {
		var[,i] = coords[[1]]$vals[var[,i]]
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
	require("ggplot2")

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
		
	# Tile
	g = geom_tile(data=var,mapping=aes(x=x,y=y,fill=value))
	
	# Axes labels
	g = c(g, scale_x_continuous(paste(coords[[1]]$name, coords[[1]]$units)), scale_y_continuous(paste(coords[[2]]$name, coords[[2]]$units)) )
	
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
	
 	# g = geom_segment(data=speeds, aes_string(x=coordsName[1], y=coordsName[2]), aes(xend=(x+scale*u), yend=(y+scale*v)))
 	g = geom_segment(data=speeds, aes(x=x, y=y, xend=(x+scale*u), yend=(y+scale*v)), arrow=arrow(length=unit(0.002,"npc"), angle=15))

	g$title = 
	
	# Add title/legend
	varnames = paste(sliceU$var$longname,sliceV$var$longname,sep="-")
	extras = sliceU$dims[!isCoord]
	extrasLegend = paste(sapply(extras,`[`,c("name","vals")),collapse=": ")
	title = paste(varnames,extrasLegend,sep="\n")
	
	return(list(g, opts(title=title)))
}
