#
#	Additions to ggplot2
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

suppressPackageStartupMessages(require("ggplot2"))

# Themes
#------------------------------------------------------------

theme_blight <- function(base_size = 12, base_family = "") {
    theme <- theme_set(theme_grey(base_size = 12, base_family = ""))

    # a few beamer colours
    beamer <- list()
    rgb(235,235,246,maxColorValue=255) -> beamer$lightBlock
    rgb(214,215,239,maxColorValue=255) -> beamer$lightBlockHeader
    rgb(141,145,208,maxColorValue=255) -> beamer$defaultAddedBlue

    theme$panel.background =   element_rect(fill = beamer$lightBlockHeader, colour = NA)
    theme$panel.grid.major =   element_line(colour = beamer$lightBlock)
    theme$panel.grid.minor =   element_line(colour = beamer$lightBlock, size = 0.25)
    theme$strip.background =   element_rect(beamer$defaultAddedBlue, colour=NA)
    theme$strip.text.x =       element_text(size = base_size * 0.8, colour = "white")
    theme$strip.text.y =       element_text(size = base_size * 0.8, angle = -90, colour = "white")

    return(theme)
}

theme_simple <- function(base_size = 12, base_family = "") {
    theme <- theme_set(theme_grey(base_size = 12, base_family = ""))

    theme$legend.background = element_blank()
    theme$legend.key =        element_blank()
    theme$panel.background =  element_blank()
    theme$panel.border =      element_blank()
    theme$panel.grid.major =  element_blank()
    theme$panel.grid.minor =  element_blank()
    theme$strip.background =  element_blank()
    theme$strip.text.x =      element_text(family = base_family, size = base_size * 0.8, face = "bold")
    theme$strip.text.y =      element_text(family = base_family, size = base_size * 0.8, face = "bold", angle = -90)

    return(theme)
}


# Plot aspect and limits
#------------------------------------------------------------
ggplot.init <- function(...)
#
#	Initializes a ggplot with orthonormal aspect ratio
#
{
	p = ggplot(...) + coord_equal() + opts(aspect.ratio = 1)
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
		l = c(l, geom_segment(data=x, mapping=aes(x=lon, y=lat, xend=lon+u*scaleFactor, yend=lat+v*scaleFactor, colour=depth), arrow=arrow(length=unit(0.008,"npc"),angle=15), ...),  scale_colour_gradient(low=beamer$lightBlue, high=beamer$darkBlue, ...))
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

ggviolin <- function(mapping, data, ...)
#
#	Geom-like function to draw violin plots with ggplot2. Analogous to boxplots.
#	x		grouping factor, on the x axis
#	y		variable
#	...     passed to density and to geom_polygon
#
{
	x <- deparse(mapping$x)
	y <- deparse(mapping$y)

	# consistency test
	if (is.numeric(data[,x])) {
	   stop("geom_violin requires a discrete variable on the x axis")
	}

	# compute density
	d <- ddply(data, x, function(X, y, ...) {
	    # compute density
	    dens <- density(X[,y], na.rm=TRUE, ...)
		# scale to a max of 0.45
		dens$y <- dens$y / max(dens$y) * 0.45
		# compute the symetry to get a full polygon
		data.frame(x=c(dens$x, rev(dens$x)), y=c(dens$y, -rev(dens$y)))
	}, y=y, ...)

	# rename columns according to their real meaning
	d <- rename(d, c(x=y, y="dens"))

	# compute the fake x scale
	d[,x] <- factor(d[,x], exclude=NULL)
	xlevels <- levels(d[,x])
	xshift <- as.numeric(d[,x])
	d$dens <- xshift + d$dens

	# build geom and set correct scale
	g = geom_polygon(data=d, mapping=aes_string(x="dens", y=y, group=x), ...)
	s = scale_x_continuous(name=y, breaks=1:length(xlevels), labels=xlevels)

	return(list(g,s))
}
