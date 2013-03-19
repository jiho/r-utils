#
#      Demo of lib_movie.R functions
#
#  (c) Copyright 2013 Jean-Olivier Irisson
#      GNU General Public License v3
#
#--------------------------------------------------------------------------

source("lib_movie.R")

# open a sequence of images
img("foo")

# plot something in it
x <- seq(1, 50, by=0.1)
for (i in 1:50) {
  plot(x, sin((x+i)/pi))
}

# close the sequence of images
dev.off()

# encode it into movies
encode.movie(name="movie_divx.mp4", codec="divx")
encode.movie(name="movie_xvid.mp4", codec="xvid", fps=20)
encode.movie(name="movie_h264.mp4", codec="h264", fps=15, verbose=T, clean=T)
# the last one has clean=T which removes the original images
