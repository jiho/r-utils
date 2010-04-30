#
#	Useful plotting functions (for geographical plots in particular)
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

# Plot limits
#------------------------------------------------------------
plot.limits <- function(data=NULL, lon=NA, lat=NA, border=0.03, ...)
#
#	Compute the minimum plotting domain for the data
#
#	data		data.frame with components lon,lat, or x,y, or as first and second columns
#	lon		vector of longitudes
#	lat		vector of latitudes
#	border	size of the border around the data points
#
#	Output a vector suitable for use as "limits" or "xlim" and "ylim" argument of other functions
{
	# extract arguments
	if (!is.null(data)) {
		if ( all( c("lon","lat")%in%names(data) ) ) {
			lon = data$lon
			lat = data$lat
		} else if ( all( c("x","y")%in%names(data) ) ) {
			lon = data$x
			lat = data$y
		} else {
			lon = data[,1]
			lat = data[,2]
		}
	}

	# compute limits
	minLat = min(lat,na.rm=T) - border
	maxLat = max(lat,na.rm=T) + border
	minLon = min(lon,na.rm=T) - border
	maxLon = max(lon,na.rm=T) + border

	return(c(minLon,maxLon,minLat,maxLat))
}

crop <- function(data, limits)
#
#	Restrict information into given limits
#
#	data		data.frame with components lon, lat, or x,y, or as first and second columns
{
	if (!is.null(limits)) {
		if ( all( c("lon","lat")%in%names(data) ) ) {
			data = data[data$lon>=limits[1] & data$lon<=limits[2] & data$lat>=limits[3] & data$lat<=limits[4],]
		} else if ( all( c("x","y")%in%names(data) ) ) {
			data = data[data$x>=limits[1] & data$x<=limits[2] & data$y>=limits[3] & data$y<=limits[4],]
		} else {
			data = data[data[,1]>=limits[1] & data[,1]<=limits[2] & data[,2]>=limits[3] & data[,2]<=limits[4],]
		}
	}
	return(x)
}


# Grid functions
#------------------------------------------------------------
vpgrid <- function(...)
#
#	Create a grid of viewports
#
#	... 					passed to grid.layout:
#	nrow, ncol			division of space
#	widths, heights	geometry (vectors of column widths and row heights)
{
	grid.newpage()
	pushViewport(viewport(layout=grid.layout(...)))
}

vplayout <- function(x, y)
#
#	Select a (group of) viewports in a grid
#
#	x,y	coordinates of the viewport(s) to select
{
	viewport(layout.pos.row=x, layout.pos.col=y)
}


frame <- function(x)
#
#	Write the text string 'x' on a blank frame and on the console
#	Useful to separate some graphics in a multipage pdf, or some test results in a console log file
#	NB: overrides the existing frame function which is an alias to plot.new anyway
#
#	x		a character string (a title, etc.)
{
	cat("\n\n# ",x,"\n",rep("-",nchar(x)+2),"\n\n",sep="")
	plot.new()
	grid.text(x, gp=gpar(fontsize=20, col=beamer$grey))
}


# Quickly save graphics
#------------------------------------------------------------

s <- function(name, prefix=".", type=c("pdf","png"), paper=c("special", "beamer", "keynote", "latex"), width=4, height=3, units=c("in", "cm", "px"), dpi=NA_real_, pointsize=9, ...) {

    # get arguments
    type <- match.arg(type)
    paper <- match.arg(paper)
    units <- match.arg(units)

    # create file name
    filename <- paste(prefix, "/", name, ".", type, sep="")
    filename <- path.expand(filename)

    # override width and height (+ units) in case paper is specified
    if (paper == "beamer") {
        width <- 4
        height <- 3
        units <- "in"
        dpi <- 72*2
    } else if (paper == "keynote"){
        scale <- 1.2
        width <- 4 * scale
        height <- 3 * scale
        units <- "in"
        dpi <- 72 * scale * 1.8
    } else if (paper == "latex"){
        width <- 5
        height <- 3
        units <- "in"
        dpi <- 72 * 5
    }

    # convert sizes to inches
    if (units == "cm") {
        width <- width / 2.54
        height <- height / 2.54
    } else if (units == "px") {
        width <- width / dpi
        height <- height / dpi
    }

    # print current graphics display
    dev.print(quartz, file=filename, type=type,
              width=width, height=height, dpi=dpi, pointsize=pointsize, ...
    )

    invisible()
}

print.dev <- function(file="Rplots.pdf", size="beamer", unit="cm", m=2, pointsize=7, ...) {
    name <- substr(file, 1, nchar(file)-3)
    type <- substr(file, nchar(file)-2, nchar(file))
    pointsize=9
    s(name=name, type=type, paper=size, units=unit, ...)
}


# openGL plots
#------------------------------------------------------------
compute.aspect <- function(x=seq(0,1,len=nrow(z)), y=seq(0,1,len=ncol(z)), z, expand=100, degree=FALSE, ...)
#
#	Compute realistic 3d aspect ratio for persp3d and the like
#
#	x,y,z		coordinates and value
#	expand	expansion factor in z direction
#	degree	wether the coordinates are measured in degrees of lat and lon (modifies the conversion)
#
{
	if (is.list(x)) {
		z = x$z
		y = x$y
		x = x$x
	}

	Xaspect = diff(range(x,na.rm=T))
	Yaspect = diff(range(y,na.rm=T))
	baseAspect = min(Xaspect,Yaspect)

	Zaspect = diff(range(z,na.rm=T)) * expand

	if (degree) {
		# in case x and y are in degrees, we need to have real measures the aspect
		# 1 degree in lat/lon is 60 minutes, 1 minute is a nautical mile (1852 m)
		# we convert this to meters to scale the z axis
		Zaspect = Zaspect/(1852*60)
	}
	aspect = c(Xaspect,Yaspect,Zaspect)
	aspect = aspect/baseAspect
	return(aspect)
}

persp.jo <- function(x=seq(0,1,len=nrow(z)), y=seq(0,1,len=ncol(z)), z, limits=NULL, overplot=FALSE, theta=10, phi=30, col=heat.colors(100), expand=1, ...)
#
#	Perspective plot with correct x,y scale for lat-lon plots
#
#	x, y		coordinates
#	z			values
#	limits	x and y limits for the plot
# 	overplot	whether to plot on to of a previous plot (hence suppressing the box around the plot)
#	theta,phi angles of view
#	color		vector of color to drape on the plot
#	expand	expansion factor for the z axis
#	...		passed to persp
#
{
	if (is.list(x)) {
		z = x$z
		y = x$y
		x = x$x
	}
	# choose color scale
	colMatrix = drape.color(z,col=col)

	# compute aspect ratio
	if (is.null(limits)) {
		limits = c(range(x),range(y))
	}
	# in case we want a real world measure map, we need to have real measures for expand
	# 1 degree in lat/lon is 60 minutes, 1 minute is a nautical mile (1852 m)
	# we convert this to meters to scale the z axis and multiply by a scaling factor to see something
	if (expand!=1) {
		expand = 1/(1852*60) * expand
	}

	# plot perspective
	res = persp(x, y, z, xlab="lon", ylab="lat", box=!overplot, ticktype="detailed", theta=theta, phi=phi, col=colMatrix, expand=expand, xlim=limits[1:2], ylim=limits[3:4], ...)
	return(res)
}



persp3d.jo <- function(x=seq(0,1,len=nrow(z)), y=seq(0,1,len=ncol(z)), z, color=heat.colors(100), xlim=NULL, ylim=NULL, zlim=NULL, ...)
#
#	Perspective plot using openGL
#
#	x, y		coordinates
#	z			values
#	color		vector of color to drape on the plot
#	x/y/zlim	limits of the plotting window (strange behavior)
#	...		passed to persp3d
#
{
	if (is.list(x)) {
		z = x$z
		y = x$y
		x = x$x
	}

	# define color scale
	color = drape.color(z, color)

	# define limits
	if (is.null(xlim)) {
		xlim = range(x,na.rm=T)
	}
	if (is.null(ylim)) {
		ylim = range(y,na.rm=T)
	}
	cat(zlim)
	if (is.null(zlim)) {
		zlim = range(z,na.rm=T)
	}

	# define aspect ratio
	aspect = compute.aspect(xlim, ylim, zlim, ...)

	# plot
	suppressPackageStartupMessages(require("rgl"))
	persp3d(x, y, z, color=color, xlim=xlim, ylim=ylim, zlim=zlim, aspect=aspect, ...)
}
