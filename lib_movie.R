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
	extension = match.arg(extension,c("jpg","png"))
	
	# build file name
	name = paste(formatC(i, width=4, flag="0"), pattern,".",extension,sep="")
	
	library("Cairo")
	
	# close the previous device if it was a Cairo device opened here, so that the function can be called in a loop and produce successive images
	if (names(dev.cur())=="Cairo" & length(dev.list())>=2) {
		dev.off()
	}
	
	# open a new one
	Cairo(file=name, type=extension, w=800*size.multi, h=592*size.multi, background="white", res=NA)
}

encode.movie <- function(name=paste(format(Sys.time(),"%Y%m%d"),".avi",sep=""), extension="png", pattern="", fps=4, bitrate=2000, codec="divx", clean=F)
#
#	Encode a sequence of images into a movie using mencoder
#	http://www.mplayerhq.hu/
#
#	name			name of the output movie, extension determines movie type
#	extension	type of the images to encode
#	pattern		pattern to look for in the names of the images to encode
#	fps			frame per second of the movie
#	bitrate		bitrate of the movive (higher = better quality)
#	codec			video codec
#	clean			whether to remove image files when done
#
{
	extension = match.arg(extension,c("jpg","png"))
	codec = match.arg(codec,c("divx","xvid"))
	
	# codec selection
	if (codec=="divx") {
		videoOpts=paste("-ovc lavc -lavcopts vcodec=mpeg4:vbitrate=",bitrate," -ffourcc divx",sep="")
	} else if (codec=="xvid") {
		# NB causes issues at the start of the video
		videoOpts=paste("-ovc xvid -xvidencopts bitrate=",bitrate," -ffourcc xvid",sep="")
	}
	
	# encoding
	commandLine = paste("mencoder mf://*",pattern,"*.",extension," -mf fps=",fps,":type=",extension," ",videoOpts," -nosound -o ",name,sep="")
	cat(commandLine)
	system(commandLine)
	
	# cleaning
	if (clean) {
		system(paste("rm -f *",pattern,"*.",extension,sep=""))
	}
}
