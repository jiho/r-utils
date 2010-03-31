#
#	Plotting functions to encode a series of images into a movie
#
#	(c) 2007-2009 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

seq.image <- function(i, extension="png", pattern="", size.multi=1)
#
#	Open a new file with a sequential name and plot a bitmap inside it
#	The function is meant to be includd in a loop. It will close the previous image and open a new one.
#
#	i				index to be converted to a sequential name
#	extension	type fo the file to plot in (png, jpeg)
#	pattern		pattern to add to the filename, before the extension
#	size.multi	size multiplier. base size is 800*592
#
{
    warning("DEPRECATED - Use png(%05d,png) or img() before a loop")

	extension = match.arg(extension,c("jpg","png"))

	# build file name
	name = paste(formatC(i, width=4, flag="0"), pattern,".",extension,sep="")

	require("Cairo")

	# close the previous device if it was a Cairo device opened here, so that the function can be called in a loop and produce successive images
	if (names(dev.cur())=="Cairo" & length(dev.list())>=2) {
		dev.off()
	}

	# open a new one
	Cairo(file=name, type=extension, w=800*size.multi, h=592*size.multi, background="white", res=NA)
}


img <- function(pattern="", extension=c("png", "jpeg", "jpg"), width=800, height=592, ...)
#
#   Open a bitmap device to plot a sequence of images (used to encode a movie)
#
#	extension	type of the file to plot
#	pattern		pattern to add to the filename, before the extension
#	size.multi	size multiplier. base size is 800*592
#
{
    # get arguments
	extension = match.arg(extension)

    # compute the size
    # should be multiples of 16 for best compression
    wR <- width %% 16
    if (wR != 0) {
        width <- as.integer(width - wR)
        warning("Width was rounded to the nearest 16 px for better compression")
    }
    hR <- height %% 16
    if (hR != 0) {
        height <- as.integer(height - hR)
        warning("Height was rounded to the nearest 16 px for better compression")
    }

    # setup name
    name <- paste(pattern, "%09d.", extension, sep="")

    # open the device
    if (extension == "png") {
        png(filename=name, width=width, height=height, ...)
    } else if (extension == "jpg") {
        jpeg(filename=name, width=width, height=height, ...)
    }
    return(invisible(name))
}

encode.movie <- function(name=paste(format(Sys.time(),"%Y%m%d-%H%M-"),codec,".mp4",sep=""), pattern="", extension=c("png", "jpg"), fps=6, codec=c("mpeg4","h264","h264lossless","divx","xvid"), clean=F, verbose=F)
#
#	Encode a sequence of images into a movie using ffmpeg
#
#	name        name of the output movie, extension determines movie type
#	extension   type of the images to encode
#	pattern     pattern to look for in the names of the images to encode
#	fps         frame per second of the movie
#	codec       video codec
#	clean       whether to remove image files when done
#
{
    # detect arguments
	extension <- match.arg(extension)
	codec <- match.arg(codec)
	if (codec %in% c("divx","xvid")) {
	   warning("DiVX or XViD are treated as generic mpeg-4 for better compatibility.")
	   codec <- "mpeg4"
	}
	if (fps < 5) {
	   warning("FPS lower than 5 is not supported")
	   fps <- 5
	}

	# create temporary file list
	tmp <- tempdir()
	imgFiles <- system(paste("ls ", pattern, "*.", extension, sep=""), intern=T)
	count <- 1
	for (file in imgFiles) {
	    system(paste("ln ", file, " ", tmp, "/encode-", sprintf("%09d", count), ".", extension, sep=""))
	    count <- count+1
	}

	# codec selection
	if (codec=="mpeg4") {
	    opts <- "-b 4000k -bt 8000k"
	} else if (codec=="h264") {
	    opts <- "-vcodec libx264 -vpre hq -qmin 1 -crf 0"
	} else if (codec=="h264lossless") {
		opts <-  "-vcodec libx264 -vpre lossless_ultrafast"
	}

	# encoding
	commandLine = paste("ffmpeg -r ", fps, " -i ", tmp, "/encode-", "%09d.", extension, " ", opts, " ", name, sep="")
	cat("\n",commandLine, "\n\n")
	system(commandLine)

	# cleaning
	system(paste("rm -f ", tmp, "/encode-*", sep=""))
	if (clean) {
        system(paste("rm -f ", pattern, "*.", extension, sep=""))
	}
}
