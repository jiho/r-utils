#
#	Beamer color palette
#
#	(c) 2007 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

# Default colors
rgb( 24, 28, 87,maxColorValue=255) -> DefaultOuter1
rgb( 37, 43,130,maxColorValue=255) -> DefaultOuter2
rgb( 49, 57,174,maxColorValue=255) -> DefaultOuter3
rgb(233,233,242,maxColorValue=255) -> DefaultBlock
rgb(141,145,208,maxColorValue=255) -> DefaultAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> DefaultBlockHeader
rgb(245,231,231,maxColorValue=255) -> DefaultAlertBlock
rgb(205,130,130,maxColorValue=255) -> DefaultAddedRed
rgb(165, 29, 30,maxColorValue=255) -> DefaultAlertBlockHeader
rgb(232,238,230,maxColorValue=255) -> DefaultExampleBlock
rgb(138,166,120,maxColorValue=255) -> DefaultAddedGreen
rgb( 43, 93, 10,maxColorValue=255) -> DefaultExampleBlockHeader
rgb(228,228,228,maxColorValue=255) -> DefaultCoveredText
rgb(224,225,243,maxColorValue=255) -> DefaultCoveredBullet
rgb(255,255,255,maxColorValue=255) -> DefaultBackground
rgb(  0,  0,  0,maxColorValue=255) -> DefaultText
beamerDefault = c(DefaultOuter1,
                   DefaultOuter2,
                   DefaultBlockHeader,
                   DefaultAddedBlue,
                   DefaultCoveredBullet,
                   DefaultAlertBlockHeader,
                   DefaultAddedRed,
                   DefaultAlertBlock,
                   DefaultExampleBlockHeader,
                   DefaultAddedGreen,
                   DefaultExampleBlock)

# Default light colors (seashore, rose)
rgb(194,195,230,maxColorValue=255) -> DefaultLightOuter1
rgb(204,205,234,maxColorValue=255) -> DefaultLightOuter2
rgb(214,215,239,maxColorValue=255) -> DefaultLightOuter3
rgb(235,235,246,maxColorValue=255) -> DefaultLightBlock
rgb(214,215,239,maxColorValue=255) -> DefaultLightBlockHeader
rgb(102,106,194,maxColorValue=255) -> DefaultLightBlockHeaderText
rgb(249,231,231,maxColorValue=255) -> DefaultLightAlertBlock
rgb(244,206,206,maxColorValue=255) -> DefaultLightAlertBlockHeader
rgb(221, 42, 43,maxColorValue=255) -> DefaultLightAlertBlockHeaderText
rgb(233,241,230,maxColorValue=255) -> DefaultLightExampleBlock
rgb(210,229,204,maxColorValue=255) -> DefaultLightExampleBlockHeader
rgb( 62,130, 19,maxColorValue=255) -> DefaultLightExampleBlockHeaderText
rgb(228,228,228,maxColorValue=255) -> DefaultLightCoveredText
rgb(224,225,243,maxColorValue=255) -> DefaultLightCoveredBullet
rgb(255,255,255,maxColorValue=255) -> DefaultLightBackground
rgb(  0,  0,  0,maxColorValue=255) -> DefaultLightText
beamerLight = c(DefaultLightBlockHeaderText,
	                     DefaultLightOuter1,
	                     DefaultLightOuter3,
	                     DefaultLightBlock,
	                     DefaultLightAlertBlockHeaderText,
	                     DefaultLightAlertBlockHeader,
	                     DefaultLightAlertBlock,
	                     DefaultLightExampleBlockHeaderText,
	                     DefaultLightExampleBlockHeader,
	                     DefaultLightExampleBlock)
	
# Beetle theme
rgb( 61, 73,112,maxColorValue=255) -> BeetleOuter1
rgb( 64, 77,119,maxColorValue=255) -> BeetleOuter2
rgb( 67, 81,125,maxColorValue=255) -> BeetleOuter3
rgb( 85,103,158,maxColorValue=255) -> BeetleAddedBlue1
rgb(115,130,179,maxColorValue=255) -> BeetleAddedBlue2
rgb(180,185,203,maxColorValue=255) -> BeetleAddedBlue3
rgb(205,130,130,maxColorValue=255) -> BeetleAddedRed
rgb(188, 33, 34,maxColorValue=255) -> BeetleAlertBlockHeaderText
rgb(138,166,120,maxColorValue=255) -> BeetleAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> BeetleExampleBlockHeaderText
rgb(255,255,255,maxColorValue=255) -> BeetleHeaderText
rgb(204,204,204,maxColorValue=255) -> BeetleAddedGrey1
rgb(168,168,168,maxColorValue=255) -> BeetleCoveredBullet
rgb(153,153,153,maxColorValue=255) -> BeetleBackground
rgb(130,130,130,maxColorValue=255) -> BeetleCoveredText
rgb( 77, 77, 77,maxColorValue=255) -> BeetleAddedGrey2
rgb(  0,  0,  0,maxColorValue=255) -> BeetleText
beamerBeetle = c(BeetleOuter1,
	              BeetleAddedBlue1,
	              BeetleAddedBlue2,
	              BeetleAddedBlue3,
	              BeetleAlertBlockHeaderText,
	              BeetleAddedRed,
	              BeetleExampleBlockHeaderText,
	              BeetleAddedGreen,
	              BeetleAddedGrey2,
	              BeetleCoveredText,
	              BeetleBackground,
	              BeetleCoveredBullet,
	              BeetleAddedGrey1)

# Albatross theme
rgb(  0,  8, 55,maxColorValue=255) -> AlbatrossOuter1
rgb(  0, 10, 75,maxColorValue=255) -> AlbatrossOuter2
rgb(  0, 13, 93,maxColorValue=255) -> AlbatrossOuter3
rgb(  0, 17,124,maxColorValue=255) -> AlbatrossBackground
rgb(  0, 15,112,maxColorValue=255) -> AlbatrossBlock
rgb(  0, 13, 93,maxColorValue=255) -> AlbatrossBlockHeader
rgb(108,112,184,maxColorValue=255) -> AlbatrossHeaderText
rgb(190,193,252,maxColorValue=255) -> AlbatrossBullet
rgb( 27, 36,143,maxColorValue=255) -> AlbatrossCoveredBullet
rgb( 62, 60, 95,maxColorValue=255) -> AlbatrossCoveredText
rgb(223,143,135,maxColorValue=255) -> AlbatrossAddedRed
rgb(202, 68, 54,maxColorValue=255) -> AlbatrossAlertBlockHeaderText
rgb(157,222,103,maxColorValue=255) -> AlbatrossAddedGreen
rgb(105,182, 40,maxColorValue=255) -> AlbatrossExampleBlockHeaderText
rgb(234,227,133,maxColorValue=255) -> AlbatrossText
rgb(218,207, 47,maxColorValue=255) -> AlbatrossAddedYellow
rgb(255,255,255,maxColorValue=255) -> AlbatrossAddedWhite

# Fly theme
rgb(  0,  0,  0,maxColorValue=255) -> FlyText
rgb( 64, 64, 64,maxColorValue=255) -> FlyAddedGrey
rgb(128,128,128,maxColorValue=255) -> FlyOuter
rgb(153,153,153,maxColorValue=255) -> FlyBackground
rgb(204,204,204,maxColorValue=255) -> FlyAddedGrey
rgb(255,255,255,maxColorValue=255) -> FlyHeaderText
rgb(168,168,168,maxColorValue=255) -> FlyCoveredBullet
rgb(130,130,130,maxColorValue=255) -> FlyCoveredText
rgb(221,144,144,maxColorValue=255) -> FlyAddedRed
rgb(188, 33, 34,maxColorValue=255) -> FlyAlertBlockHeaderText
rgb(156,189,134,maxColorValue=255) -> FlyAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> FlyExampleBlockHeaderText
rgb(152,156,215,maxColorValue=255) -> FlyAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> FlyAddedDefaultBlue

# Seagull theme
rgb(179,179,179,maxColorValue=255) -> SeagullOuter1
rgb(191,191,191,maxColorValue=255) -> SeagullOuter2
rgb(204,204,204,maxColorValue=255) -> SeagullOuter3
rgb(230,230,230,maxColorValue=255) -> SeagullBlock
rgb(217,217,217,maxColorValue=255) -> SeagullAddedGrey
rgb(204,204,204,maxColorValue=255) -> SeagullBlockHeader
rgb(220,220,220,maxColorValue=255) -> SeagullCoveredText
rgb(247,247,247,maxColorValue=255) -> SeagullCoveredBullet
rgb(255,255,255,maxColorValue=255) -> SeagullBackground
rgb(  0,  0,  0,maxColorValue=255) -> SeagullText

# Beaver theme
rgb(141, 25, 25,maxColorValue=255) -> BeaverOuterFrame
rgb(173, 95, 95,maxColorValue=255) -> BeaverAddedRed
rgb(204,166,166,maxColorValue=255) -> BeaverAddedRed
rgb(217,217,217,maxColorValue=255) -> BeaverOuter1
rgb(236,236,236,maxColorValue=255) -> BeaverOuter2
rgb(242,242,242,maxColorValue=255) -> BeaverOuter3
rgb(143,147,205,maxColorValue=255) -> BeaverAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> BeaverBlockHeaderText
rgb(147,180,125,maxColorValue=255) -> BeaverAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> BeaverExampleBlockHeaderText
rgb(200,138,138,maxColorValue=255) -> BeaverAddedRed
rgb(164, 40, 40,maxColorValue=255) -> BeaverAlertBlockHeaderText
rgb(228,228,228,maxColorValue=255) -> BeaverCoveredText
rgb(224,225,243,maxColorValue=255) -> BeaverCoveredBullet
rgb(255,255,255,maxColorValue=255) -> BeaverBackground
rgb(  0,  0,  0,maxColorValue=255) -> BeaverText

# Crane theme
rgb(240,193, 57,maxColorValue=255) -> CraneOuter1
rgb(243,200, 74,maxColorValue=255) -> CraneOuter2
rgb(244,206, 94,maxColorValue=255) -> CraneOuter3
rgb(251,237,196,maxColorValue=255) -> CraneBlock
rgb(245,212,121,maxColorValue=255) -> CraneAddedOrange
rgb(239,187, 46,maxColorValue=255) -> CraneBlockHeader
rgb(245,231,231,maxColorValue=255) -> CraneAlertBlock
rgb(205,130,130,maxColorValue=255) -> CraneAddedRed
rgb(165, 29, 30,maxColorValue=255) -> CraneAlertBlockHeader
rgb(232,238,230,maxColorValue=255) -> CraneExampleBlock
rgb(138,166,120,maxColorValue=255) -> CraneAddedGreen
rgb( 43, 93, 10,maxColorValue=255) -> CraneExampleBlockHeader
rgb(228,228,228,maxColorValue=255) -> CraneCoveredText
rgb(224,225,243,maxColorValue=255) -> CraneCoveredBullet
rgb(  0, 12, 74,maxColorValue=255) -> CraneBullet
rgb(255,255,255,maxColorValue=255) -> CraneBackground
rgb(  0,  0,  0,maxColorValue=255) -> CraneText

# Wolverine theme
rgb(236,161, 79,maxColorValue=255) -> WolverineOuter1
rgb(242,188, 46,maxColorValue=255) -> WolverineOuter2
rgb(248,219, 48,maxColorValue=255) -> WolverineOuter3
rgb(250,227, 48,maxColorValue=255) -> WolverineOuter4
rgb(253,241,152,maxColorValue=255) -> WolverineAddedYellow
rgb(140,147,211,maxColorValue=255) -> WolverineAddedBlue
rgb( 25, 39,166,maxColorValue=255) -> WolverineHeaderText
rgb(163,197,146,maxColorValue=255) -> WolverineAddedGreen
rgb( 72,139, 36,maxColorValue=255) -> WolverineExampleBlockTitle
rgb(228,228,228,maxColorValue=255) -> WolverineCoveredText
rgb(224,225,243,maxColorValue=255) -> WolverineCoveredBullet
rgb(255,255,255,maxColorValue=255) -> WolverineBackground
rgb(  0,  0,  0,maxColorValue=255) -> WolverineText


# Aliases
beamerDarkBlue = DefaultOuter1
beamerBlue = DefaultBlockHeader
beamerLightBlue = DefaultLightOuter1
beamerDarkRed = DefaultAlertBlockHeader
beamerRed = DefaultLightAlertBlockHeaderText
beamerLightRed = DefaultLightAlertBlockHeader
beamerDarkGreen = DefaultExampleBlockHeader
beamerGreen = DefaultLightExampleBlockHeaderText
beamerLightGreen = DefaultLightExampleBlockHeader
beamerDarkGrey = BeetleAddedGrey2
beamerGrey = BeetleCoveredText
beamerLightGrey = BeetleAddedGrey1
beamers = c(beamerDarkBlue,
				beamerBlue,
	         beamerLightBlue,
	         beamerDarkRed,
	         beamerRed,
	         beamerLightRed,
	         beamerDarkGreen,
	         beamerGreen,
	         beamerLightGreen,
	         beamerDarkGrey,
	         beamerGrey,
	         beamerLightGrey)


#	Functions generating color ramps
#------------------------------------------------------------

beamer.blues <- function(n, alpha = 1)
#	Generate n shades of blue
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamerDarkBlue, beamerLightBlue))(n), alpha=alpha))
}

beamer.greens <- function(n, alpha = 1)
# 	Generate n shades of green
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamerDarkGreen, beamerLightGreen))(n), alpha=alpha))
}

beamer.reds <- function(n, alpha = 1)
# 	Generates n shades of red
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamerDarkRed, beamerLightRed))(n), alpha=alpha))
}

beamer.greys <- function(n, alpha = 1)
# 	Generates n shades of grey
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamerDarkGrey, beamerLightGrey))(n), alpha=alpha))
}


# 	ggplot2 themes
#------------------------------------------------------------

# beamer light
theme_blight = list(
	grid.fill=DefaultLightBlockHeader, 
	grid.minor.colour=DefaultLightBlock, 
	grid.colour=DefaultLightBlock, 
	strip.gp=gpar(fill=DefaultAddedBlue, col="white", lwd=2)
)

# beamer white
theme_bwhite = list(
	grid.fill="white", 
	grid.minor.colour="white", 
	grid.colour="white", 
	strip.gp=gpar(fill=DefaultAddedBlue, col="white", lwd=2)
)

