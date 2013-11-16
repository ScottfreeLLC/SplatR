##############################################################
#
# Package   : splatr
# Module    : variable.functions
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

# define the variable functions

month <- function(date)
{
   months(splatr.chrondate(date))
}

day <- function(date)
{
   days(splatr.chrondate(date))
}

quarter <- function(date)
{
   quarters(splatr.chrondate(date))
}

year <- function(date)
{
   years(splatr.chrondate(date))
}

week <- function(date)
{
   weeks(splatr.chrondate(date))
}

weekday <- function(date)
{
   weekdays(splatr.chrondate(date))
}

hour <- function(time)
{
   hours(splatr.chrontime(time))
}

minute <- function(time)
{
   minutes(splatr.chrontime(time))
}

second <- function(time)
{
   seconds(splatr.chrontime(time))
}

percent <- function(xn, xd)
{
	100 * (xn / xd)
}

pchange <- function(xn, xd)
{
	100 * (xn / xd - 1)
}

rangeindex <- function(x, high, low)
{
	percent(x - low, high - low)
}

gap <- function(open, close)
{
	close.1 <- splatr.shift(close, -1)
	pchange(open, close.1)
}

net <- function(x, period)
{
   if (missing(period) | period == 0)
      period <- 1
   x - splatr.shift(x, -period)
}

netreturn <- function(x, period)
{
   if (missing(period) | period == 0)
      period <- 1
	pchange(x, splatr.shift(x, -period))
}

truehigh <- function(high, low)
{
   plow <- splatr.shift(low, -1)
	ifelse(high < plow, plow, high)
}

truelow <- function(high, low)
{
   phigh <- splatr.shift(high, -1)
	ifelse(low > phigh, phigh, low)
}

truerange <- function(high, low)
{
	(truehigh(high, low) - truelow(high, low))
}

sepoc <- function(open, high, low, close)
{
	rangeindex(close, high, low) - rangeindex(open, high, low)
}

sepol <- function(open, high, low)
{
	rangeindex(open, high, low)
}

sepoh <- function(open, high, low)
{
	100 - sepol(open, high, low)
}

sepcl <- function(high, low, close)
{
	rangeindex(close, high, low)
}

sepch <- function(high, low, close)
{
	100 - sepcl(high, low, close)
}

nextlower <- function(x, v)
{
	xl <- x[x < v]
	i <- which(abs(xl - v) == min(abs(xl - v)))
	which(x == xl[i])
}

nexthigher <- function(x, v)
{
	xh <- x[x > v]
	i <- which(abs(xh - v) == min(abs(xh - v)))
	which(x == xh[i])
}

barnn <- function(x)
{
	n <- length(x)
	y <- numeric(n)
	for (i in 1:n)
	{
		nl <- nextlower(x[1:i], x[i])
		if (nl > 0)
			y[x[i]] <- x[i] - x[nl]
		else
			y[x[i]] <- x[i]
	}
	y
}

barhigh <- function(x)
{
	xo <- rev(order(x))
	barnn(xo)
}

barlow <- function(x)
{
	xo <- order(x)
	barnn(xo)
}

nbarhigh <- function(x, n)
{
	barhigh(x) >= n
}

nbarlow <- function(x, n)
{
	barlow(x) >= n
}
