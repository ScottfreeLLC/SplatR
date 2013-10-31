splatr.newposition <-
	function(name,
			  subject = "stock",
			  date = date(),
			  frame = splatr.newframe("price", "states", "d1"))
{
	pdata <- splatr.getframe(name, frame)
	oid <- splatr.nparse(name, "oid")
	oclass <- splatr.nparse(name, "oclass")
	dir <- splatr.getdirectory(oid, oclass)
	mdata <- splatr.dlookup(dir, oid)
	if (is.null(mdata)) {
		cat("Could not find metadata for", name, "\n")
		NULL
	}
	else {
		value <- new("splatr.position",
					   name = name,
					   subject = subject,
					   gframe = frame,
					   startdate = date,
					   status = "new",
					   pdata = pdata,
					   mdata = mdata,
					   gclass = "position",
					   mclass = "trade",
					   dynamic = TRUE,
					   recursive = FALSE,
					   ...)
		value
	}
   rm(frame)
}

splatr.addposition <- function(position, trade)
{
	vp <- splatr.validposition(position)
	vt <- splatr.validtrade(trade)
	if (vp & vt)
		added <- splatr.addgroup(position, trade)
	else {
		if (!vp)
			cat("Invalid position", position, "\n")
		if (!vt)
			cat("Invalid trade", trade, "\n")
		cat("Could not add", trade@name, "to position\n")
		added <- FALSE
	}
	added
}

splatr.removeposition <-
	function(position,
			   trade)
{
	vp <- splatr.validposition(position)
	vt <- splatr.validtrade(trade)
	if (vp & vt) {
		success <- splatr.removegroup(position, trade)
		if (success) rm(trade)
	}
	else {
		if (!vp)
			cat("Invalid position", position, "\n")
		if (!vt)
			cat("Invalid trade", trade, "\n")
		cat("Could not remove", trade@name, "from position\n")
		success <- FALSE
	}
	success
}

#
# Example of Cost Basis:
#
# | +100 | * 10 =  1,000
# | +200 | * 15 =  3,000
# | -500 | * 20 = 10,000
# --------        ------
#    800          14,000  =>  14,000 / 800 = 17.5
#
#    Position is -200 (net short) @ 17.5
#

splatr.valuateposition <-
	function(position,
			   date = date())
{
	pdata <- position@pdata
	cp <- pdata[date == date, close]
	mdata <- position@mdata
	multiplier <- mdata$multiplier
	netpos <- 0
	tts <- 0		# total traded shares
	ttv <- 0		# total traded value
	totalprofit <- 0.0
	pgroup <- position@trades
	trades <- pgroup@member
	for (i in seq(along = trades))
	{
		trade <- trades[i]
		tq <- trade@quantity
		netpos <- netpos + tq
		tts <- tts + abs(tq)
		tp <- trade@price
		pfactor <- tq * multiplier
		cv <- pfactor * cp
		cvabs <- abs(cv)
		ttv <- ttv + cvabs
		ev <- pfactor * tp
		totalprofit <- totalprofit + cv - ev
	}
	position@quantity <- netpos
	position@price <- cp
	position@value <- abs(netpos) * multiplier * cp
	position@profit <- totalprofit
	position@costbasis <- ttv / tts
	position@return <- splatr.pchange(totalprofit, cvabs)
	position@value
}

splatr.clearposoffsets <- function(position)
{
	trades <- position@members
	for (i in seq(along = trades))
		trades[i]@qoffset <- 0
}