#
#	Additions to ggplot2
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>. 
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------


# Themes
#------------------------------------------------------------

# store the current theme to go back to it afterwards
old = theme_get()

# beamer light theme
theme_set(theme_gray())	# use default theme as reference
theme_update(
	panel.background = theme_rect(fill = DefaultLightBlockHeader, colour=NA),
	panel.grid.major = theme_line(colour = DefaultLightBlock),
	panel.grid.minor = theme_line(colour = DefaultLightBlock),
	strip.background = theme_rect(fill = DefaultAddedBlue, colour = "white", size=2)
)
# store the full theme: default + modifs in a variable
theme_blight = theme_get()

# beamer white
# beamer light theme
theme_set(theme_gray())	# use default theme as reference
theme_update(
	panel.background = theme_blank(),
	panel.grid.major = theme_blank(),
	panel.grid.minor = theme_blank(),
	strip.background = theme_rect(fill = DefaultAddedBlue, colour = "white", size=2)
)
# store the full theme: default + modifs in a variable
theme_bwhite = theme_get()

# go back to state before doing all that
theme_set(old)



# Plot aspect and limits
#------------------------------------------------------------
ggplot.init <- function(...)
#
#	Initializes a ggplot with orthonormal aspect ratio
#
{
	require("ggplot2")
	p = ggplot(...) + coord_equal()
	p$aspect.ratio = 1
	return(p)
}

gginit <- function(...) {
	return(ggplot.init(...))
}

ggadd.limits <- function(limits, ...)
#
#	Set the limits of a ggplot
#
#	limits	xmin, xmax, ymin, ymax as with regular R graphics
{
	require("ggplot2")
	return(list(
		scale_x_continuous(limits=limits[1:2]),
		scale_y_continuous(limits=limits[3:4]))
	)
}


# New "geoms"
#------------------------------------------------------------

ggadd.arrows <- function(x=NULL, lon=x$lon, lat=x$lat, u=x$u, v=x$v, depth=x$depth, duration=20, ...)
#
#	Plot a field as arrows
#	ARGUMENTS
#	x				a speed field, with components: lon, lat, u and v
#	OR
#	lat, lon		locations of arrows starts (in degrees)
#	u, v			speeds (in cm/s)
#	depth			depths of measurements (optional)
#	duration 	the lengths of the arrows are equivalent to the drift from the origin during `duration` minutes
#
{
	library("ggplot2")
	
	if (is.null(x)) {
		x = data.frame(lon=lon, lat=lat, u=u, v=v, depth=depth)		
	}
	
	if (is.null(x$depth)) {
		withDepth = FALSE
	} else {
		withDepth = TRUE
	}
	
	# Add legend data as the last line of the data
	# TODO: find a way to do that that is compatible with facetting
	# lU = -40
	# lV = 0
	# lLon = max(x$lon)
	# lLat = min(x$lat-0.05)
	# lText = paste(lU,"cm s-1")
	# x = rbind(x,NA)
	# if (withDepth) {
	# 	x = x[order(x$depth, decreasing=TRUE),]
	# }
	# x[nrow(x),c("u","v","lon","lat")] = c(lU,lV,lLon,lLat)
	# if (withDepth) {
	# 	lDepth = max(x$depth, na.rm=T)
	# 	x[nrow(x),c("depth")] = lDepth
	# }

	# Compute scale factor
	scaleFactor = 60*duration/1112000
	# 	1112000 = one degree of latitude in cm
	# Add it to the data.frame (because ggplot can't find it otherwise)
	x$scaleFactor = scaleFactor
	
	# Plot arrows
	l = list()
	if (withDepth) {
		l = c(l, geom_segment(data=x, mapping=aes(x=lon, y=lat, xend=lon+u*scaleFactor, yend=lat+v*scaleFactor, colour=depth), arrow=arrow(length=unit(0.008,"npc"),angle=15), ...),  scale_colour_gradient(low=beamerLightBlue, high=beamerDarkBlue, ...))	
	} else {
		l = c(l, geom_segment(data=x, mapping=aes(x=lon, y=lat, xend=lon+u*scaleFactor, yend=lat+v*scaleFactor), arrow=arrow(length=unit(0.008,"npc"),angle=15), ...))		
	}

 	# Add legend text
	# l = c(l, geom_text(data=cbind(x[nrow(x),],label=lText), aes(x=lon, y=lat, label=label), hjust=0.6, vjust=-1))
	
	return(l)
}

draw_range <- function(x, y, colour="black", ...)
#
#	A 'geom-like' function which computes the mean of y at all x points and draws the mean and mean+sd ribbon
#
{
	# consistencey check
	if (length(x)!=length(y)) {
		stop("x and y must be the same length in draw_range")
	}
	
	xM = data.frame(pressure=x, value=y)
	
	# compute mean and sd of the variable
	means = cast(xM,pressure~.,fun=mean, na.rm=T)
	sds = cast(xM,pressure~.,fun=sd, na.rm=T)
	xM = data.frame(pressure=means[,1], mean=means[,2], sd=sds[,2])

	# plot them
	g = list(geom_ribbon(data=xM, mapping=aes(x=pressure, min=(mean-sd), max=(mean+sd)), fill=lighter(colour,20), colour=NA, ...), geom_path(data=xM, mapping=aes(x=pressure, y=mean), colour=colour, ...))
	
	return(g)
}

geom_violin <- function(data, mapping, bw="nrd0", adjust=1, kernel="gaussian", ...)
#
#	geom-like function to draw violin plots with ggplot2. Analogous to boxplots
#	default aesthetics:
#	x		grouping factor, on the x axis
#	y		variable
#	arguments passed to density
#	bw		
#	adjust
#	kernel
#	...	passed to density and to geom_polygon
#
{
	x = deparse(mapping$x)
	y = deparse(mapping$y)
	molten = melt(data, measure.var=y)
	nbByX = cast(molten, formula=paste(x, "~ variable"), fun.aggregate=length)
	
	# remove levels for which there are less thant 2 points because density estimate fails
	molten = molten[molten[[x]]%in%(nbByX[nbByX[y]>2,x]),]
	molten[[x]] = factor(molten[[x]])	# remove potentially absent factor levels?
	
	# densities = cast(molten, formula=paste(x, "~ variable"), fun.aggregate=density)
	# why doesn't this work?
	# do it 'by hand'
	
	moltenL = split(molten$value, molten[[x]])
	
	densities = lapply(moltenL, function(x, bw, adjust, kernel, ...){
		# compute density
		out = density(x, bw=bw, adjust=adjust, kernel=kernel, ...)
		out = data.frame(y=out$x, dens=out$y)
		# scale density
		out$dens = out$dens/max(out$dens) * 0.45 # max is 0.45
		# duplicate it to make a nice polygon
		out2 = data.frame(y=rev(out$y), dens=-rev(out$dens))
		out = rbind(out,out2,NA)
	}, bw=bw, adjust=adjust, kernel=kernel, ...)
	
	# x scale has step of 1
	for (i in 1:length(densities)) {
		densities[[i]]$dens = i + densities[[i]]$dens
	}
	xLabels = names(densities)
	densities = do.call("rbind",densities)
	
	# build geom and set correct scale
	g = geom_polygon(data=densities, mapping=aes(x=dens, y=y), ...)
	s = scale_x_continuous(breaks=1:length(xLabels), labels=xLabels)

	return(list(g,s))
}
