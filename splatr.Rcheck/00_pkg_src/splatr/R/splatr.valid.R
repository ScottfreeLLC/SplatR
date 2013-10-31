splatr.validstat <- function(object)
{
	# tbd
}

splatr.validframe <- function(object)
{
	# fdata must be a valid identifier
	# fclass must be a valid class name
	# fractal must be a character and number code
}

splatr.validdirectory <- function(object)
{
	# name must be a valid character string
	# class must be a valid class
	# uri must be an accessible uri
	# key must be a valid variable name
}

splatr.validfeed <- function(object)
{
	# name must be a valid character string
	# class must be a valid class
	# uri must be an accessible uri
	# key must be a valid variable name
}

splatr.validmethod <- function(object)
{
	# tbd
}

splatr.validstates <- function(object)
{
	# name must be a valid name
	# frame must be a valid Frame
	# content must be a valid class name
	# key must be a slot in the given class or an item name of a list
}

splatr.validgroup <- function(object)
{
	# gclass must be a valid class name
	# mclass must be a valid class name
	# dynamic must be TRUE (default) or FALSE
	# recursive must be TRUE or FALSE (default)
	# members must be a set of valid mclass objects
	# frame must be a valid frame
	# gstates must be a valid States object
	# mstates must be a valid States object
}

splatr.validtrade <- function(object)
{
	# date must be a valid date/time
	# quantity must be a number, + or -
	# name must be a valid symbol
	# price must be a valid positive number
	# qoffset must be a valid integer, + or -
	TRUE
}

splatr.validposition <- function(object)
{
	# name must be a valid identifier
	# quantity must be a number, + for long and - for short
	# price must be a positive fpn (sig. 2)
	# status must be a character string: 1 of 3 values (see below)
	# pdata must be a valid time series
	# mdata must be a valid list
	# value must be a valid fpn
	# profit must be an fpn, positive or negative
	# costbasis must be an fpn
	# return must be a valid percentage, positive or negative
	TRUE
}

splatr.validportfolio <- function(object)
{
	# cash can be any positive floating point number (significance 2)
	# margin must be a valid percentage {0.0 - 1.0}
	# mincash must be a valid percentage
	# fixedfrac is the fraction (percentage) of the account
	# maxpos must be a positive integer
	# posby is a valid variable
	# maxloss must be a positive percentage
	# kopos must be an integer >= 0 and < maxpos
	# koby must be a valid variable name
	# restricted must be TRUE (default) or FALSE
	# pmatch must be TRUE (exact) or [FALSE (any)]
	# psearch must be [TRUE (fifo)] or FALSE (lifo)
	# weightby must be a valid variable name
	# weights must be a vector of percentages totalling 1.0
	# startdate must be a valid date
	# startcap must be a positive floating point number (significance 2)
	# value must be a positive fpn
	# profit must be a fpn: positive or negative
	# return must be a valid percentage: positive or negative
	TRUE
}

splatr.validanalysis <- function(object)
{
	# tbd
}
