##############################################################
#
# Package   : splatr
# Module    : utility
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getsmart <- function(name)
{
	tryCatch(get(name),
			  error = function(e) NULL)
}

splatr.getclass <- function(name)
{
	object <- splatr.getsmart(name)
	if (is.null(object))
		"character"
	else
		class(object)
}

splatr.getdate <- function(daysback = 0)
{
	cdate <- as.character(strptime(Sys.time(), "%Y-%m-%d"))
	chrondate <- chron(dates = cdate, format="y-m-d")
	chrondate - daysback
}

splatr.chrondate <- function(date)
{
   dts <- dates(as.character(date), format = c(dates = "y-m-d"))
   chron(dates = dts)
}

splatr.chrontime <- function(time)
{
   tms <- times(as.character(time), format = c(times = "h:m:s"))
   chron(times = tms)
}

splatr.shift <- function(x, shiftby) 
{
	stopifnot (is.numeric(shiftby))
	if (length(shiftby) > 1)
		return (sapply(shiftby, splatr.shift, x=x))
	out <- NULL
	absshiftby <- abs(shiftby)
	if (shiftby > 0)
		out <- c(tail(x, -absshiftby), rep(NA, absshiftby))
	else if (shiftby < 0 )
		out <- c(rep(NA, absshiftby), head(x, -absshiftby))
	else
		out <- x
	out
}