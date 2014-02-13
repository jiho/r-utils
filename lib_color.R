#
#	Functions to deal with colors
#
# (c) 2009 Jean-Olivier Irisson <irisson@normalesup.org>. 
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

# Visualize colors
#------------------------------------------------------------
contact.sheet <- function(colors)
#	Visualizes given colors
{
	n = length(colors)
	image(1:n, 1, as.matrix(1:n), col=colors, xlab="", ylab="", xaxt="n", yaxt="n", bty="n")
}


# Manipulate colors
#------------------------------------------------------------
change.level <- function(color, offset)
#
#	Change the level of a color (make it lighter or darker)
#
#	color		original color
#	offset	[-100,100] = -100 cuts to black, 100 cuts to white 
{
	suppressPackageStartupMessages(require("grDevices"))
	# ensure offset is within [-100,100]
	offset = min(max(offset,-100),100)
	# create a palette
	# NB: it would be more natural to manipulate colors in HLS space (Hue, Level, Saturation) and move the "Level" component. However there is currently no easy way to do this in R currently so we fake it with a color ramp from black to white.
	colors = colorRampPalette(c("black", color, "white"))(201)
	# select a color with the offset
	return(colors[101+offset])
}

lighten <- function(color, offset=40)
#	Make a color lighter
{
	return(change.level(color, offset))
}

darken <- function(color, offset=40)
#	Make a color darker
{
	return(change.level(color, -offset))
}

change.saturation <- function(color, offset)
#
#	Change the saturation of a color (make is flashier or duller)
#
#	color		original color
#	offset	[-100,100] = -100 turns into grey, 100 maximizes saturation
{
	suppressPackageStartupMessages(require("grDevices"))
	# convert to HSV space
	rgbCol = col2rgb(color)
	hsvCol = rgb2hsv(rgbCol)
	# ensure offset is within [-100,100]
	offset = min(max(offset,-100),100)
	# create a palette
	# NB: it would be more natural to manipulate colors in HLS space (Hue, Level, Saturation) and move the "Saturation" component. However there is currently no easy way to do this in R currently so we fake it with a color ramp.
	colors = colorRampPalette(c(hsv(hsvCol[1,], 0, hsvCol[3,]), color, hsv(hsvCol[1,], 1, hsvCol[3,])))(201)	
	# select a color with the offset
	return(colors[101+offset])
}

saturate <- function(color, offset=40)
#	Saturate a color
{
	return(change.saturation(color, offset))
}

desaturate <- function(color, offset=40)
#	Decrease the saturation of a color
{
	return(change.saturation(color, -offset))
}


# Color scales
#------------------------------------------------------------
jet.colors <- colorRampPalette(c("#00007F", "blue", "#007FFF", "cyan", "#7FFF7F", "yellow", "#FF7F00", "red", "#7F0000"))


# Plotting related functions
#------------------------------------------------------------
drape.color <- function(x, col)
#
#	Compute a matrix of colors of dimensions = dim(x) in which the intensity of the color is proportional to the value in x.
#	For use with persp, as the color parameter
#
#	x			input matrix
#	col		a vector of colors, usually a gradient
{
	# compute value at the center of each facet
	nrx <- nrow(x)
    ncx <- ncol(x)
	xfacet <- x[-1, -1] + x[-1, -ncx] + x[-nrx, -1] + x[-nrx, -ncx]
	# assign color
	n <- length(col)
	facetCol <- cut(xfacet, n)
	facetCol <- col[facetCol]
	return(facetCol)
}


