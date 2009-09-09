#
#	Color palette matching the latex Beamer class
#		http://latex-beamer.sourceforge.net/
#
#	(c) 2007-2009 Jean-Olivier Irisson <irisson@normalesup.org>.
#	GNU General Public License http://www.gnu.org/copyleft/gpl.html
#
#------------------------------------------------------------

beamer = list()

# Default colors
rgb( 24, 28, 87,maxColorValue=255) -> beamer$defaultOuter1
rgb( 37, 43,130,maxColorValue=255) -> beamer$defaultOuter2
rgb( 49, 57,174,maxColorValue=255) -> beamer$defaultOuter3
rgb(233,233,242,maxColorValue=255) -> beamer$defaultBlock
rgb(141,145,208,maxColorValue=255) -> beamer$defaultAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> beamer$defaultBlockHeader
rgb(245,231,231,maxColorValue=255) -> beamer$defaultAlertBlock
rgb(205,130,130,maxColorValue=255) -> beamer$defaultAddedRed
rgb(165, 29, 30,maxColorValue=255) -> beamer$defaultAlertBlockHeader
rgb(232,238,230,maxColorValue=255) -> beamer$defaultExampleBlock
rgb(138,166,120,maxColorValue=255) -> beamer$defaultAddedGreen
rgb( 43, 93, 10,maxColorValue=255) -> beamer$defaultExampleBlockHeader
rgb(228,228,228,maxColorValue=255) -> beamer$defaultCoveredText
rgb(224,225,243,maxColorValue=255) -> beamer$defaultCoveredBullet
rgb(255,255,255,maxColorValue=255) -> beamer$defaultBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$defaultText
beamer$default = c(beamer$defaultOuter1,
                   beamer$defaultOuter2,
                   beamer$defaultBlockHeader,
                   beamer$defaultAddedBlue,
                   beamer$defaultCoveredBullet,
                   beamer$defaultAlertBlockHeader,
                   beamer$defaultAddedRed,
                   beamer$defaultAlertBlock,
                   beamer$defaultExampleBlockHeader,
                   beamer$defaultAddedGreen,
                   beamer$defaultExampleBlock)

# Default light colors (seashore, rose)
rgb(194,195,230,maxColorValue=255) -> beamer$lightOuter1
rgb(204,205,234,maxColorValue=255) -> beamer$lightOuter2
rgb(214,215,239,maxColorValue=255) -> beamer$lightOuter3
rgb(235,235,246,maxColorValue=255) -> beamer$lightBlock
rgb(214,215,239,maxColorValue=255) -> beamer$lightBlockHeader
rgb(102,106,194,maxColorValue=255) -> beamer$lightBlockHeaderText
rgb(249,231,231,maxColorValue=255) -> beamer$lightAlertBlock
rgb(244,206,206,maxColorValue=255) -> beamer$lightAlertBlockHeader
rgb(221, 42, 43,maxColorValue=255) -> beamer$lightAlertBlockHeaderText
rgb(233,241,230,maxColorValue=255) -> beamer$lightExampleBlock
rgb(210,229,204,maxColorValue=255) -> beamer$lightExampleBlockHeader
rgb( 62,130, 19,maxColorValue=255) -> beamer$lightExampleBlockHeaderText
rgb(228,228,228,maxColorValue=255) -> beamer$lightCoveredText
rgb(224,225,243,maxColorValue=255) -> beamer$lightCoveredBullet
rgb(255,255,255,maxColorValue=255) -> beamer$lightBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$lightText
beamer$light = c(beamer$lightBlockHeaderText,
                 beamer$lightOuter1,
                 beamer$lightOuter3,
                 beamer$lightBlock,
                 beamer$lightAlertBlockHeaderText,
                 beamer$lightAlertBlockHeader,
                 beamer$lightAlertBlock,
                 beamer$lightExampleBlockHeaderText,
                 beamer$lightExampleBlockHeader,
                 beamer$lightExampleBlock)

# Beetle theme
rgb( 61, 73,112,maxColorValue=255) -> beamer$beetleOuter1
rgb( 64, 77,119,maxColorValue=255) -> beamer$beetleOuter2
rgb( 67, 81,125,maxColorValue=255) -> beamer$beetleOuter3
rgb( 85,103,158,maxColorValue=255) -> beamer$beetleAddedBlue1
rgb(115,130,179,maxColorValue=255) -> beamer$beetleAddedBlue2
rgb(180,185,203,maxColorValue=255) -> beamer$beetleAddedBlue3
rgb(205,130,130,maxColorValue=255) -> beamer$beetleAddedRed
rgb(188, 33, 34,maxColorValue=255) -> beamer$beetleAlertBlockHeaderText
rgb(138,166,120,maxColorValue=255) -> beamer$beetleAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> beamer$beetleExampleBlockHeaderText
rgb(255,255,255,maxColorValue=255) -> beamer$beetleHeaderText
rgb(204,204,204,maxColorValue=255) -> beamer$beetleAddedGrey1
rgb(168,168,168,maxColorValue=255) -> beamer$beetleCoveredBullet
rgb(153,153,153,maxColorValue=255) -> beamer$beetleBackground
rgb(130,130,130,maxColorValue=255) -> beamer$beetleCoveredText
rgb( 77, 77, 77,maxColorValue=255) -> beamer$beetleAddedGrey2
rgb(  0,  0,  0,maxColorValue=255) -> beamer$beetleText
beamer$beetle = c(beamer$beetleOuter1,
	              beamer$beetleAddedBlue1,
	              beamer$beetleAddedBlue2,
	              beamer$beetleAddedBlue3,
	              beamer$beetleAlertBlockHeaderText,
	              beamer$beetleAddedRed,
	              beamer$beetleExampleBlockHeaderText,
	              beamer$beetleAddedGreen,
	              beamer$beetleAddedGrey2,
	              beamer$beetleCoveredText,
	              beamer$beetleBackground,
	              beamer$beetleCoveredBullet,
	              beamer$beetleAddedGrey1)

# Albatross theme
rgb(  0,  8, 55,maxColorValue=255) -> beamer$albatrossOuter1
rgb(  0, 10, 75,maxColorValue=255) -> beamer$albatrossOuter2
rgb(  0, 13, 93,maxColorValue=255) -> beamer$albatrossOuter3
rgb(  0, 17,124,maxColorValue=255) -> beamer$albatrossBackground
rgb(  0, 15,112,maxColorValue=255) -> beamer$albatrossBlock
rgb(  0, 13, 93,maxColorValue=255) -> beamer$albatrossBlockHeader
rgb(108,112,184,maxColorValue=255) -> beamer$albatrossHeaderText
rgb(190,193,252,maxColorValue=255) -> beamer$albatrossBullet
rgb( 27, 36,143,maxColorValue=255) -> beamer$albatrossCoveredBullet
rgb( 62, 60, 95,maxColorValue=255) -> beamer$albatrossCoveredText
rgb(223,143,135,maxColorValue=255) -> beamer$albatrossAddedRed
rgb(202, 68, 54,maxColorValue=255) -> beamer$albatrossAlertBlockHeaderText
rgb(157,222,103,maxColorValue=255) -> beamer$albatrossAddedGreen
rgb(105,182, 40,maxColorValue=255) -> beamer$albatrossExampleBlockHeaderText
rgb(234,227,133,maxColorValue=255) -> beamer$albatrossText
rgb(218,207, 47,maxColorValue=255) -> beamer$albatrossAddedYellow
rgb(255,255,255,maxColorValue=255) -> beamer$albatrossAddedWhite

# Fly theme
rgb(  0,  0,  0,maxColorValue=255) -> beamer$flyText
rgb( 64, 64, 64,maxColorValue=255) -> beamer$flyAddedGrey
rgb(128,128,128,maxColorValue=255) -> beamer$flyOuter
rgb(153,153,153,maxColorValue=255) -> beamer$flyBackground
rgb(204,204,204,maxColorValue=255) -> beamer$flyAddedGrey
rgb(255,255,255,maxColorValue=255) -> beamer$flyHeaderText
rgb(168,168,168,maxColorValue=255) -> beamer$flyCoveredBullet
rgb(130,130,130,maxColorValue=255) -> beamer$flyCoveredText
rgb(221,144,144,maxColorValue=255) -> beamer$flyAddedRed
rgb(188, 33, 34,maxColorValue=255) -> beamer$flyAlertBlockHeaderText
rgb(156,189,134,maxColorValue=255) -> beamer$flyAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> beamer$flyExampleBlockHeaderText
rgb(152,156,215,maxColorValue=255) -> beamer$flyAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> beamer$flyAddedDefaultBlue

# Seagull theme
rgb(179,179,179,maxColorValue=255) -> beamer$seagullOuter1
rgb(191,191,191,maxColorValue=255) -> beamer$seagullOuter2
rgb(204,204,204,maxColorValue=255) -> beamer$seagullOuter3
rgb(230,230,230,maxColorValue=255) -> beamer$seagullBlock
rgb(217,217,217,maxColorValue=255) -> beamer$seagullAddedGrey
rgb(204,204,204,maxColorValue=255) -> beamer$seagullBlockHeader
rgb(220,220,220,maxColorValue=255) -> beamer$seagullCoveredText
rgb(247,247,247,maxColorValue=255) -> beamer$seagullCoveredBullet
rgb(255,255,255,maxColorValue=255) -> beamer$seagullBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$seagullText

# Beaver theme
rgb(141, 25, 25,maxColorValue=255) -> beamer$beaverOuterFrame
rgb(173, 95, 95,maxColorValue=255) -> beamer$beaverAddedRed
rgb(204,166,166,maxColorValue=255) -> beamer$beaverAddedRed
rgb(217,217,217,maxColorValue=255) -> beamer$beaverOuter1
rgb(236,236,236,maxColorValue=255) -> beamer$beaverOuter2
rgb(242,242,242,maxColorValue=255) -> beamer$beaverOuter3
rgb(143,147,205,maxColorValue=255) -> beamer$beaverAddedBlue
rgb( 49, 57,174,maxColorValue=255) -> beamer$beaverBlockHeaderText
rgb(147,180,125,maxColorValue=255) -> beamer$beaverAddedGreen
rgb( 58,124, 14,maxColorValue=255) -> beamer$beaverExampleBlockHeaderText
rgb(200,138,138,maxColorValue=255) -> beamer$beaverAddedRed
rgb(164, 40, 40,maxColorValue=255) -> beamer$beaverAlertBlockHeaderText
rgb(228,228,228,maxColorValue=255) -> beamer$beaverCoveredText
rgb(224,225,243,maxColorValue=255) -> beamer$beaverCoveredBullet
rgb(255,255,255,maxColorValue=255) -> beamer$beaverBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$beaverText

# Crane theme
rgb(240,193, 57,maxColorValue=255) -> beamer$craneOuter1
rgb(243,200, 74,maxColorValue=255) -> beamer$craneOuter2
rgb(244,206, 94,maxColorValue=255) -> beamer$craneOuter3
rgb(251,237,196,maxColorValue=255) -> beamer$craneBlock
rgb(245,212,121,maxColorValue=255) -> beamer$craneAddedOrange
rgb(239,187, 46,maxColorValue=255) -> beamer$craneBlockHeader
rgb(245,231,231,maxColorValue=255) -> beamer$craneAlertBlock
rgb(205,130,130,maxColorValue=255) -> beamer$craneAddedRed
rgb(165, 29, 30,maxColorValue=255) -> beamer$craneAlertBlockHeader
rgb(232,238,230,maxColorValue=255) -> beamer$craneExampleBlock
rgb(138,166,120,maxColorValue=255) -> beamer$craneAddedGreen
rgb( 43, 93, 10,maxColorValue=255) -> beamer$craneExampleBlockHeader
rgb(228,228,228,maxColorValue=255) -> beamer$craneCoveredText
rgb(224,225,243,maxColorValue=255) -> beamer$craneCoveredBullet
rgb(  0, 12, 74,maxColorValue=255) -> beamer$craneBullet
rgb(255,255,255,maxColorValue=255) -> beamer$craneBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$craneText

# Wolverine theme
rgb(236,161, 79,maxColorValue=255) -> beamer$wolverineOuter1
rgb(242,188, 46,maxColorValue=255) -> beamer$wolverineOuter2
rgb(248,219, 48,maxColorValue=255) -> beamer$wolverineOuter3
rgb(250,227, 48,maxColorValue=255) -> beamer$wolverineOuter4
rgb(253,241,152,maxColorValue=255) -> beamer$wolverineAddedYellow
rgb(140,147,211,maxColorValue=255) -> beamer$wolverineAddedBlue
rgb( 25, 39,166,maxColorValue=255) -> beamer$wolverineHeaderText
rgb(163,197,146,maxColorValue=255) -> beamer$wolverineAddedGreen
rgb( 72,139, 36,maxColorValue=255) -> beamer$wolverineExampleBlockTitle
rgb(228,228,228,maxColorValue=255) -> beamer$wolverineCoveredText
rgb(224,225,243,maxColorValue=255) -> beamer$wolverineCoveredBullet
rgb(255,255,255,maxColorValue=255) -> beamer$wolverineBackground
rgb(  0,  0,  0,maxColorValue=255) -> beamer$wolverineText


# Aliases
beamer$darkBlue = beamer$defaultOuter1
beamer$blue = beamer$defaultBlockHeader
beamer$lightBlue = beamer$lightOuter1
beamer$darkRed = beamer$defaultAlertBlockHeader
beamer$red = beamer$lightAlertBlockHeaderText
beamer$lightRed = beamer$lightAlertBlockHeader
beamer$darkGreen = beamer$defaultExampleBlockHeader
beamer$green = beamer$lightExampleBlockHeaderText
beamer$lightGreen = beamer$lightExampleBlockHeader
beamer$darkGrey = beamer$beetleAddedGrey2
beamer$grey = beamer$beetleCoveredText
beamer$lightGrey = beamer$beetleAddedGrey1


#	Functions generating color ramps
#------------------------------------------------------------

beamer.blues <- function(n, alpha = 1)
#	Generate n shades of blue
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamer$darkBlue, beamer$lightBlue))(n), alpha=alpha))
}

beamer.greens <- function(n, alpha = 1)
# 	Generate n shades of green
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamer$darkGreen, beamer$lightGreen))(n), alpha=alpha))
}

beamer.reds <- function(n, alpha = 1)
# 	Generates n shades of red
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamer$darkRed, beamer$lightRed))(n), alpha=alpha))
}

beamer.greys <- function(n, alpha = 1)
# 	Generates n shades of grey
{
	require(graphics)
	require(ggplot2)
	return(alpha(colorRampPalette(c(beamer$darkGrey, beamer$lightGrey))(n), alpha=alpha))
}
