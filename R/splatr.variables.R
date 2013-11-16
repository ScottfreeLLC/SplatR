##############################################################
#
# Package   : splatr
# Module    : variables
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.variables <<- list(
   month = "month(date)",
   day = "day(date)",
   quarter = "quarter(date)",
   year = "year(date)",
   week = "week(date)",
   weekday = "weekday(date)",
   hour = "hour(time)",
   minute = "minute(time)",
   second = "second(time)",
	higherclose = "close > close.1",
	lowerclose =  "close < close.1",
	gapup =   "open > close.1",
	gapdown = "open < close.1",
	higherhigh = "high > high.1",
	lowerlow   = "low < low.1",
	lowerhigh  = "high < high.1",
	higherlow  = "low > low.1",
	higheropen = "open > open.1",
	loweropen  = "open < open.1",
	bagapup   = "open > high.1",
	bagapdown = "open < low.1",
	cma = "mean(close)",
	truerange = "truerange(high, low)",
	truehigh =  "truehigh(high, low)",
	truelow =   "truelow(high, low)",
	net = "net(close, period)",
   netup = "net > 0",
   netdown = "net < 0",
	netreturn = "netreturn(close, period)",
	atr = "mean(truerange)",
	vma = "mean(volume)",
	volatility = "percent(atr, close)",
   highestopen = "max(open)",
   highesthigh = "max(high)",
   highestlow = "max(low)",
   highestclose = "max(close)",
   lowestopen = "min(open)",
   lowesthigh = "min(high)",
   lowestlow = "min(low)",
   lowestclose = "min(close)",
   range = "high - low",
	sepoc = "sepoc(open, high, low, close)",
	sepol = "sepol(open, high, low)",
	sepoh = "sepoh(open, high, low)",
	sepcl = "sepcl(high, low, close)",
	sepch = "sepch(high, low, close)",
   sepocabs = "abs(sepoc)",
   highsep = "sepocabs >= 70.0",
   rangeratio = "range / atr20",
   highrange = "rangeratio >= 1.5",
   lowrange = "rangeratio <= 0.7",
   bigup = "highrange & highsep & netup",
   bigdown = "highrange & highsep & netdown",
   volratio = "atr3 / atr20"
)

splatr.lead.variables <<-
   c("open", "gapup", "gapdown", "higheropen", "loweropen", "bagapup", "bagapdown")


