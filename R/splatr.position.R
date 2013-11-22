##############################################################
#
# Package   : splatr
# Module    : position
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getpositionname <-
   function(pname, name)
{
   pname <- paste(pname, name, sep=".")
   pname
}

splatr.setposition <-
   function(pname, pobject)
{
   assign(pname, pobject, envir = .GlobalEnv)
   pname
}

splatr.getposition <-
   function(pname)
{
   splatr.getref(pname)
}

splatr.newposition <-
	function(portname, name, opendate)
{
   # initialize position fields
	status <- "flat"
	quantity <- 0
	if (missing(opendate))
	   opendate <- splatr.getdate()
   openprice <- 0.0
   openvalue <- 0.0
	date <- opendate
	price <- 0.0
   value <- 0.0   
	duration <- date - opendate
   profit <- 0.0
   costbasis <- 0.0
   netreturn <- 0.0
   mae <- 0.0
   mfe <- 0.0 
	trades <- list()
   ntrades <- 0
   # extract portfolio information
	portfolio <- splatr.getportfolio(portname)
   subject <- portfolio$subject
   space <- portfolio$space
   pname <- splatr.getframename(name, subject, space)
	pdata <- splatr.getref(pname)
	mdata <- splatr.getglobalstates("splatr", "multiplier")
   multiplier <- mdata[[subject]]
	phigh <- splatr.getstate(pdata, opendate, "high")
	plow <- splatr.getstate(pdata, opendate, "low")
	# create position object
	pnew <- new("splatr.position",
               name = name,
	            status = status,
	            quantity = quantity,
	            opendate = opendate,
               openprice = openprice,
               openvalue = openvalue,
	            date = date,
	            price = price,
               value = value,
	            duration = duration,
	            profit = profit,
	            costbasis = costbasis,
	            netreturn = netreturn,
               phigh = phigh,
               plow = plow,
	            mae = mae,
	            mfe = mfe,
	            trades = trades,
	            ntrades = ntrades,
	            pname = pname,
	            multiplier = multiplier)
   if (!is.null(pnew)) {
      posname <- splatr.getpositionname(portname, name)
      splatr.setposition(posname, pnew)      
   }
   posname
}

splatr.updateposition <-
   function(portfolio, position, trade)
{
   position$trades <- c(position$trades, trade)
   position$ntrades <- position$ntrades + 1
   position$date <- trade$tdate
   position$duration <- position$date - position$opendate
   value <- splatr.valuateposition(position, trade$tdate)
   if (position$quantity > 0)
      position$status <- "long"
   else if (position$quantity < 0)
      position$status <- "short"
   else
      position$status <- "flat"
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
   function(position, tdate)
{
   # get current price
	pname <- position$pname
   pdata <- splatr.getref(pname)
	cp <- splatr.getstate(pdata, tdate, "close")
   # update mfe and mae
	ch <- splatr.getstate(pdata, tdate, "high")
   if (ch > position$phigh)
      position$phigh <- ch
	cl <- splatr.getstate(pdata, tdate, "low")
   if (cl < position$plow)
      position$plow <- cl
	if (position$status == "short") {
      position$mfe <- pchange(position$openprice, position$plow)
      position$mae <- pchange(position$openprice, position$phigh)
	}
   if (position$status == "long") {
      position$mfe <- pchange(position$phigh, position$openprice)
      position$mae <- pchange(position$plow, position$openprice)
   }
	# start valuation
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

splatr.closeposition <-
   function(portfolio, position, tdate)
{            
   # if necessary, put on an offsetting trade
   pq <- position$quantity
   if (pq != 0)
   {
      tradesize <- -pq
      position$date <- tdate
      pdata <- splatr.getref(position$pname)
      cp <- splatr.getstate(pdata, tdate, "close")
      newtrade <- splatr.newtrade(position$name,
                                  tradesize,
                                  cp,
                                  tdate)
      if (newtrade)
         splatr.updateportfolio(portfolio,
                                position,
                                newtrade,
                                tradesize)      
   }
}
