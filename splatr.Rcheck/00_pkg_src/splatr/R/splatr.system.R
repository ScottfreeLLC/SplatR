splatr.newsystem <-
	function(name,
			  leevent,
			  leorder,
		  	  lxevent,
		  	  lxorder,
			  seevent,
			  seorder,
			  sxevent,
			  sxorder)
{
	value <- new("splatr.system",
				   name = name,
				   subject = "System",
				   leevent = leevent,
				   leorder = leorder,
				   lxevent = lxevent,
				   lxorder = lxorder,
				   seevent = seevent,
				   seorder = seorder,
				   sxevent = sxevent,
				   sxorder = sxorder)
	value
}

splatr.updatesystemstate <- function(system, sframe, tframe, trade, exitfrac)
{
	tradestates <- c("le", "lx", "se", "sx")
	tamount <- trade@quantity
	for (i in seq(along=tradestates))
	{
		torder <- get(paste("system@", tradestates[i], "order", sep=""))
		order <- torder("order")
		price <- get(paste(order, "price"))
		trigger <- get(paste(order, "trigger"))
		if (!is.null(torder))
		{
			# action
			if(order == "le" | order == "sx")
				action <- "buy"
			else if (order == "se" | order == "lx")
				action <- "sell"
			else
				action <- "none"
			# order type
			if (torder("stop") & torder("limit"))
				ordertype <- "stoplimit"
			else if (torder("stop"))
				ordertype <- "stop"
			else if (torder("limit"))
				ordertype <- "limit"
			else
				ordertype <- "market"
			# calculate prices
			points <- torder("points")
			if (is.null(points))
				points <- 0.0
			if (ordertype == "stop") {
				stopprice <- with(sframe, eval(parse(text=torder("stop"))))
				if (action == "buy") {
					with(sframe, price <- pmax(stopprice + points, low))
					sexpr <- paste("high > ", stopprice)
				}
				else if (action == "sell") {
					with(sframe, price <- pmin(stopprice - points, high))
					sexpr <- paste("low < ", stopprice)
				}
				slexpr <- sexpr
			}
			else if (ordertype == "limit") {
				limitprice <- with(sframe, eval(parse(text = torder("limit"))))
				if (action == "buy") {
					with(sframe, price <- pmin(limitprice - points, high))
					lexpr <- paste("low < ", limitprice)
				}
				else if (action == "sell") {
					with(sframe, price <- pmax(limitprice + points, low))
					lexpr <- paste("high > ", limitprice)
				}
				slexpr <- lexpr
			}
			else if (ordertype == "stoplimit") {
				if (action == "buy") {
					with(sframe, price <- pmax(stopprice + points, low))
				}
				else if (action == "sell") {
					with(sframe, price <- pmin(stopprice - points, high))
				}
				slexpr <- paste(sexpr, "&", lexpr)
			}
			else if (ordertype == "market")
				slexpr <- paste(TRUE)
			with(sframe, trigger <- eval(parse(text=slexpr)))
		}
	}	# end for loop
	dstate <- splatr.getstates(system)
	for (i in 2:nrow(sframe))
	{
		trade$date <- sframe$date[i]
		statechange <- FALSE
		if (sframe$lxtrigger[i] & !sframe$lxtrigger[i-1]) {
			statechange <- TRUE
			dstate$tradestate <- "lx"
			trade@price <- sframe$lxprice[i]
			trade@quantity <- -tamount
			splatr.setstate(tframe, trade, dstate)
			dstate$marketposition <- "flat"
		}
		if (sframe$sxtrigger[i] & !sframe$sxtrigger[i-1]) {
			statechange <- TRUE
			dstate$tradestate <- "sx"
			trade@price <- sframe$sxprice[i]
			trade@quantity <- tamount
			splatr.setstate(tframe, trade, dstate)
			dstate$marketposition <- "flat"
		}
		if (sframe$letrigger[i] & !sframe$letrigger[i-1]) {
			statechange <- TRUE
			trade@price <- sframe$leprice[i]
			trade@quantity <- tamount
			if (date$marketposition == "short")
				splatr.setstate(tframe, trade, dstate)
			dstate$tradestate <- "le"
			dstate$marketposition <- "long"
			dstate$tradenumber <- dstate$tradenumber + 1
			dstate$tradepl <- 0.0
			splatr.setstate(tframe, trade, dstate)
		}
		if (sframe$setrigger[i] & !sframe$setrigger[i-1]) {
			statechange <- TRUE
			trade@price <- sframe$seprice[i]
			trade@quantity <- -tamount
			if (date$marketposition == "long")
				splatr.setstate(tframe, trade, dstate)
			dstate$tradestate <- "se"
			dstate$marketposition <- "short"
			dstate$tradenumber <- dstate$tradenumber + 1
			dstate$tradepl <- 0.0
			splatr.setstate(tframe, trade, dstate)
		}
		if (!statechange)
			dstate$marketposition <- sframe$marketposition[i-1]
		pdelta <- 0.0
		if (dstate$marketposition == "long" | dstate$tradestate == "lx") {
			if (dstate$tradestate == "le")
				pdelta <- with(sframe, close - leprice)
			else if (dstate$tradestate == "lx")
				pdelta <- with(sframe, lxprice - close[i-1])
			else
				pdelta <- with(sframe, close[i] - close[i-1])
			dstate$longpl <- dstate$longpl + pdelta
		}
		if (dstate$marketposition == "short" | dstate$tradestate == "sx") {
			if (dstate$tradestate == "se")
				pdelta <- with(sframe, seprice - close)
			else if (dstate$tradestate == "sx")
				pdelta <- with(sframe, close[i-1] - sxprice)
			else
				pdelta <- with(sframe, close[i-1] - close[i])
			dstate$shortpl <- dstate$shortpl + pdelta
		}
		dstate$tradepl <- dstate$tradepl + pdelta
		sframe[i]$marketposition <- dstate$marketposition
		if (statechange | dstate$marketposition != "flat")
			sframe$tradenumber[i] <- dstate$tradenumber
		else
			sframe$tradenumber[i] <- 0
		sframe$tradepl[i] <- dstate$tradepl
		sframe$longpl[i] <- dstate$longpl
		sframe$shortpl[i] <- dstate$shortpl
		sframe$totalpl[i] <- dstate$longpl + dstate$shortpl
	}
	sframe
}

splatr.runsystem <- function(systems, group, fractal, xfrac)
{
	pframe <- splatr.newframe("price", "states", "fractal")
	trade <- new("splatr.trade", quantity = 1)
	svars <- c("longentry", "longexit", "shortentry", "shortexit")
	gmembers <- splatr.gselect(group, "member")
	glen <- length(gmembers)
	for (i in seq(along = systems))
	{
		sframe <- splatr.newframe(systems[i]@name, "states", fractal)
		tframe <- splatr.newframe(systems[i]@name, "trades", fractal)
		Variable("longentry", systems[i]@leevent)
		Variable("longexit", systems[i]@lxevent)
		Variable("shortentry", systems[i]@seevent)
		Variable("shortexit", systems[i]@sxevent)
		for (j in seq(along=members))
		{
			trade$name <- gmembers[j]
			mps <- splatr.getframe(gmembers[j], pframe)
			if (is.null(mps))
				stop("Could not find price data:", as.character(mps), "\n")
			mss <- mps						                 # create system state file
			mss <- vapply(gmembers[j], sframe, svars)
			mss <- splatr.trackstates(mss, state)
			splatr.setframename(gmembers[j], sframe, mss)
			mt <- splatr.newstates(gmembers[j], tframe, "Trade")   # create member trade frame
			mss <- splatr.updatesystemstate(systems[i], mss, mt, trade, xfrac)
		}
	}
	gframe <- splatr.newframe("systems", "trades", fractal)
	splatr.setframe(group, frame)
	# create group trades file
	alltrades <- splatr.gapply(group, splatr.mergeframes, systems)
	splatr.setframename(group@name, gframe, alltrades)
	portfolio <- splatr.generateportfolio(group, alltrades)
	rm(gframe, pframe, sframe, tframe)
	rm(trade)
	return(portfolio)
}