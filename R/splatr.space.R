##############################################################
#
# Package   : splatr
# Module    : space
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

setRefClass("splatr.space",
	fields = list(
		what = "character",
		wherein = "character",
		fractal = "character"))
	
splatr.newspace <-
	function(what = "price",
		      wherein = "states",
			   fractal = "d1")
{
	snew <- new("splatr.space",
               what = what,
               wherein = wherein,
               fractal = fractal)
	snew
}
