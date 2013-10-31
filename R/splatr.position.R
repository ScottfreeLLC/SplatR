##############################################################
#
# Package   : splatr
# Module    : position
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getpositionname <- function(pname, name)
{
   pname <- paste(pname, name, sep=".")
   pname
}

splatr.setposition <- function(pname, pobject)
{
   assign(pname, pobject, envir = .GlobalEnv)
   pname
}

splatr.getposition <- function(pname)
{
   splatr.getref(pname)
}

splatr.newposition <-
	function(portname,
            name,
            startdate)
{
   # initialize position fields
	if (missing(startdate))
      startdate <- splatr.getdate()
   enddate <- startdate
	duration <- enddate - startdate
	quantity <- 0
	status <- "flat"
   price <- 0.0
   trades <- list()
   ntrades <- 0
   value <- 0
   profit <- 0
   costbasis <- 0
   netreturn <- 0
	mae <- 0
	mfe <- 0
   # extract portfolio information
	portfolio <- splatr.getportfolio(portname)
   subject <- portfolio$subject
   space <- portfolio$space
   pname <- splatr.getframename(name, subject, space)
   mdata <- splatr.getglobalstates("splatr", "multiplier")
   multiplier <- mdata[[subject]]
   # create position object
	pnew <- new("splatr.position",
               name = name,
	            startdate = startdate,
	            enddate = enddate,
	            duration = duration,
	            quantity = quantity,
	            status = status,
	            price = price,
	            trades = trades,
	            ntrades = ntrades,
	            pname = pname,
	            multiplier = multiplier,
	            value = value,
	            profit = profit,
	            costbasis = costbasis,
	            netreturn = netreturn,
	            mae = mae,
	            mfe = mfe)
   if (!is.null(pnew)) {
      posname <- splatr.getpositionname(portname, name)
      splatr.setposition(posname, pnew)
   }
   posname
}

splatr.updateposition <- function(position, trade)
{
   position$trades <- c(position$trades, trade)
   position$ntrades <- position$ntrades + 1
   position$enddate <- trade$tdate
   position$duration <- position$enddate - position$startdate
   value <- splatr.valuateposition(position, trade$tdate)   
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

splatr.valuateposition <- function(position, tdate)
{
	pname <- position$pname
   pdata <- splatr.getref(pname)
	cp <- splatr.getstate(pdata, tdate, "close")
	multiplier <- position$multiplier
	netpos <- 0
	tts <- 0		# total traded shares
	ttv <- 0		# total traded value
	totalprofit <- 0.0
	trades <- position$trades
	for (i in seq(along = trades))
	{
		trade <- trades[[i]]
		tq <- trade$quantity
		netpos <- netpos + tq
		tts <- tts + abs(tq)
		tp <- trade$price
		pfactor <- tq * multiplier
		cv <- pfactor * cp
		cvabs <- abs(cv)
		ttv <- ttv + cvabs
		ev <- pfactor * tp
		totalprofit <- totalprofit + cv - ev
	}
	position$quantity <- netpos
	position$price <- cp
	position$value <- abs(netpos) * multiplier * cp
	position$profit <- totalprofit
	position$costbasis <- ttv / tts
	position$netreturn <- percent(totalprofit, cvabs)
	position$value
}
