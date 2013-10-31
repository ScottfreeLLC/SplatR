setMethod("initialize", "splatr.portfolio",
	function(.Object,
			  name,
			  gclass,
			  mclass,
			  dynamic,
			  recursive,
			  ...)
{
	if (missing(dynamic))
		dynamic <- TRUE
	if (missing(recursive))
		recursive <- TRUE
	callNextMethod(.Object,
					 name = name,
					 gclass = "portfolio",
					 mclass = "position",
					 dynamic = dynamic,
					 recursive = recursive,
					 ...)
})

splatr.generateportfolio <-
	function(group,
			  tframe,
			  bperiod,
			  ...)
{
	if (is(group, "splatr.portfolio"))
		portfolio <- group
	else
		portfolio <- splatr.newportfolio(group, ...)
	success <- splatr.maketrades(portfolio, tframe, bperiod)
	if (!success)
		stop("Could not make trades in portfolio")
	portfolio
}

splatr.newportfolio <-
	function(name = "group",
			  subject = "stock",
			  gframe = splatr.newframe("price", "states", "d1"),
			  cash = 0,
			  margin = 0.5,
			  mincash = 0.2,
			  fixedfrac = 0.1,
			  maxpos = 10,
			  posby = "",
			  maxloss = 0.1,
			  kopos = 0,
			  koby = "-profit",
			  restricted = TRUE,
			  pmatch = FALSE,
			  psearch = TRUE,
			  weightby = "quantity",
			  startdate = date())
{
	value <- new("splatr.portfolio",
				   name = name,
				   subject = subject,
				   gframe = gframe,
				   cash = cash,
				   margin = margin,
				   mincash = mincash,
				   fixedfrac = fixedfrac,
				   maxpos = maxpos,
				   maxloss = maxloss,
				   kopos = kopos,
				   koby = koby,
				   restricted = restricted,
				   pmatch = pmatch,
				   psearch = psearch,
				   weightby = weightby,
				   startdate = startdate)
   rm(gframe)
	value
}

splatr.maketrades <-
	function(portfolio,
			   tframe,
			   bperiod)
{
	psvar <- portfolio@posby
	psflag <- is.null(psvar)
	bflag <- ifelse(missing(bperiod), FALSE, TRUE)
	ntrades <- 0
	for (i in 1:nrow(tframe))
	{
		if (psflag) {
			pframe <- splatr.getframe(tframe[i]$name, portfolio)
			psize <- (portfolio@value * portfolio@fixedfrac) / pframe[date == tframe[i]$date, psvar]
		}
		else
			psize <- tframe[i]$quantity
		success <- splatr.trade(portfolio,
									tframe[i]$name,
									psize,
									tframe[i]$price,
									tframe[i]$date)
		if (!success)
			stop("Trade failed for", tframe[i]$name, "in portfolio", portfolio@name, "\n")
		else
			ntrades <- ntrades + 1
		if (bflag & (ntrades %% bperiod == 0)) {
			success <- splatr.balance(portfolio, tframe[i]$date)
			if (!success)
				stop("Could not balance portfolio:", portfolio@name, "\n")
		}
	}
}

splatr.trade <-
	function(portfolio,
			  name,
			  quantity,
			  price,
			  date = date())
{
	newtrade <- new("splatr.trade",
					  name = name,
					  quantity = quantity,
					  price = price,
					  date = date,
					  ...)
	if (is.null(newtrade))
		stop("Could not create trade object")
	position <- splatr.plookup(portfolio, name)
	newpos <- is.na(position)
	if (newpos)
		position <- splatr.position(name, date, portfolio@frame)
	if (is.null(position))
		if (newpos)
			stop("Could not create Position object")
		else
			stop("Could not find Position object")
	allocation <- splatr.allocatetrade(portfolio, position, newtrade)
	if (allocation > 0)
	{
		if (newpos) {
			success <- splatr.addportfolio(portfolio, position)
			if (!success)
				stop("Could not add position to portfolio")
		}
		else
			position@status <- "open"
		success <- splatr.committrade(portfolio,
										  position,
										  newtrade,
										  allocation,
										  date)
		if (success) {
			if (position@status == "closed") {
				success <- splatr.removeportfolio(portfolio, position)
				if (!success)
					stop("Could not remove position from portfolio")
			}
		}
		else
			stop("Could not commit trade to portfolio")
	}
	else
	{
		cat("Could not allocate trade in portfolio\n")
		success <- FALSE
	}
	success
}

splatr.allocatetrade <-
	function(portfolio,
			  position,
			  trade)
{
	value <- splatr.valuateportfolio(portfolio, trade@date)
	cash <- portfolio@cash
	margin <- portfolio@margin
	mincash <- portfolio@mincash
	restricted <- portfolio@restricted
	if (restricted)
	{
		success <- splatr.kickout(portfolio, trade@date)
		if (!success)
			cat("Could not kick out positions from portfolio\n")
		success <- splatr.stoploss(portfolio, trade@date)
		if (!success)
			cat("Could not execute stop-loss portfolio procedure\n")
	}
	mdata <- position@mdata
	qpold <- position@quantity
	qtrade <- trade@quantity
	qpnew <- qpold + qtrade
	allocation <- abs(qpnew) - abs(qpold)
	addedvalue <- trade@price * mdata$multiplier * abs(allocation)
	if (restricted)
	{
		cashreserve <- mincash * cash
		freemargin <- (cash - cashreserve) / margin
		if (addedvalue > freemargin) {
			cat("Required free margin:", freemargin, "< added value:", addedvalue, "\n")
			allocation <- 0
		}
		else {
			freecash <- cash - addedvalue
			if (freecash < 0)
				portfolio@cash <- cash + freecash
		}
	}
	allocation
}

splatr.committrade <-
	function(portfolio,
			  position,
			  trade,
			  allocation,
			  date)
{
	splatr.offsettrades(portfolio, position, trade)
	success <- splatr.addposition(position, trade)
	if (success)
	{
		success <- splatr.updateportfolio(portfolio, position, allocation)
		if (!success)
			stop("Could not update position in portfolio")
		else {
			splatr.setstate(portfolio@gstates, portfolio, date)
			splatr.setstate(portfolio@mstates, position, date)
		}
	}
	else
		cat("Could not add trade to position\n")
	splatr.clearposoffsets(position)
	success
}

splatr.offsettrades <-
	function(portfolio,
			  position,
			  trade)
{
	pmatch <- portfolio@pmatch
	psearch <- portfolio@psearch
	qpold <- position@quantity
	qtrade <- trade@quantity
	qpnew <- qpold + qtrade
	qremain <- abs(qtrade)
	long <- qtrade > 0
	trades <- as.name(position@members)
	fseq <- seq(along = trades)
	rseq <- rev(fseq)
	pseq <- ifelse(psearch, fseq, rseq)
	splatr.clearoffsets(position)
	for (i in pseq)
	{
		ct <- trades[i]
		cq <- ct@quantity
		cqabs <- abs(cq)
		offsetting <- ifelse(long, cq < 0, cq > 0)
		if (offsetting) {
			if (pmatch) {
				if (cq == -qtrade) {
					ct@qoffset <- -cq
					break
				}
			}
			else {
				if (qremain <= cqabs) {
					ct@qoffset <- ifelse(long, -qremain, qremain)
					break
				}
				else {
					ct@qoffset <- -cq
					qremain <- qremain - cqabs
				}
			} # pmatch
		} # offsetting
	} # for loop
	if (qremain == 0)
		trade@qoffset <- -qtrade
	else if (qremain > 0)
		if (!pmatch) trade@quantity <- qpnew
	else
		stop("Fatal error while offsetting trades")
}

splatr.updateportfolio <-
	function(portfolio,
			  position,
			  allocation)
{
	currentcash <- portfolio@cash
	mdata <- position@mdata
	trades <- as.name(position@members)
	for (i in seq(along = trades))
	{
		trade <- trades[i]
		tradeq <- trade@quantity
		tradeo <- trade@offset
		closetrade <- (tradeq + tradeo) == 0
		if (closetrade) {
			success <- splatr.removeposition(position, trade)
			if (!success)
				stop("Could not remove trade from position")
		}
	}
	cv <- trade@price * mdata$multiplier * allocation
	portfolio@cash <- currentcash - cv
	ntrades <- length(position@members)
	if (ntrades == 0)
		position@status = "closed"
	value <- splatr.valuateposition(position, trade@date)
	success <- TRUE
	success
}

splatr.balance <-
	function(portfolio,
			  date,
			  cashlevel)
{
	currentcash <- portfolio@cash
	mincash <- portfolio@mincash
	weightby <- portfolio@weightby
	if (portfolio@pmatch) {
		cat("Cannot balance a portfolio with equal trade matching\n")
		return(FALSE)
	}
	if (missing(cashlevel))
		cashlevel <- mincash
	value <- splatr.valuateportfolio(portfolio, date)
	pvalue <- value - cashlevel * value
	positions <- splatr.getpositions(portfolio)
	npos <- length(positions)
	bdata <- numeric(npos)
	if (weightby != "") {
		plusminus <- substr(weightby, 1, 1)
		invert <- ifelse(plusminus == "-", TRUE, FALSE)
		variable <- sub("-", "", weightby)
		if (is.slot(weightby, "splatr.position"))
			bdata <- sapply(1:npos,
							  function (i) eval(substitute(positions[i]@x, x=variable)))
		else
			bdata <- sapply(1:npos,
							  function (i) positions[i]@pdata[date == date, variable])
		if (invert)
			bweights <- (2 * mean(bdata) - bdata) / sum(bdata)
		else
			bweights <- bdata / sum(bdata)
	}
	for (i in seq(along=positions))
	{
		position <- positions[i]
		mdata <- position@mdata
		pdata <- position@pdata
		bdelta <- bweights[i] * pvalue - position@value
		cp <- pdata[date == date, close]
		tradesize <- trunc(bdelta / cp)
		ntv <- abs(tradesize) * cp * mdata$multiplier
		success <- splatr.committrade(portfolio,
										  position,
										  new("splatr.trade", position@name, tradesize, cp, date),
										  0)
		if (success)
			portfolio@cash <- currentcash + bdelta - ntv
		else
			stop("Could not commit trade while rebalancing")
	}
	success
}

splatr.kickout <-
	function(portfolio,
		      date,
			   freepos)
{
	positions <- splatr.getpositions(portfolio)
	npos <- length(positions)
	kodata <- numeric(npos)
	koby <- ifelse(koby == "", "profit", koby)
	plusminus <- substr(koby, 1, 1)
	descending <- ifelse(plusminus == "-", TRUE, FALSE)
	variable <- sub("-", "", koby)
	if (is.slot(koby, "splatr.position"))
		kodata <- sapply(1:npos,
							function (i) eval(parse(text=paste("positions[i]", koby, sep="@"))))
	else
		kodata <- sapply(1:npos,
							function (i) positions[i]@pdata[date == date, variable])
	koorder <- ifelse(descending, rev(order(kodata)), order(kodata))
	if (missing(freepos)) {
		kopos <- portfolio@kopos
		maxpos <- portfolio@maxpos
		opos <- maxpos - npos
		freepos <- ifelse(opos <= 0, kopos - opos, 0)
	}
	closed <- sapply(1:freepos,
					   function(i) splatr.closeposition(portfolio, positions[koorder[i]], date))
	success <- ifelse(all(closed), TRUE, FALSE)
	success
}

splatr.stoploss <- function(portfolio, date)
{
	positions <- splatr.getpositions(portfolio)
	npos <- length(positions)
	maxloss <- portfolio@maxloss
	mldata <- sapply(1:npos, function(i) positions[i]@return)
	mlpos <- which(mldata <= -maxloss)
	closed <- sapply(1:length(mlpos),
					   function (i) splatr.closeposition(portfolio, positions[mlpos[i]], date))
	success <- ifelse(all(closed), TRUE, FALSE)
	success
}

splatr.getpositions <- function(portfolio)
{
	pmembers <- portfolio@members
	positions <- pmembers[is(pmembers, "splatr.position")]
	positions
}

splatr.addportfolio <- function(portfolio, position)
{
	vport <- splatr.validportfolio(portfolio)
	vpos <- splatr.validposition(position)
	if (vport & vpos)
		added <- splatr.addgroup(portfolio, position)
	else {
		if (!vport)
			cat("Invalid portfolio", portfolio, "\n")
		if (!vpos)
			cat("Invalid position", position, "\n")
		cat("Could not add", position@name, "to portfolio\n")
		added <- FALSE
	}
	added
}

splatr.removeportfolio <- function(portfolio, position)
{
	vport <- splatr.validportfolio(portfolio)
	vpos <- splatr.validposition(position)
	if (vport & vpos) {
		success <- splatr.removegroup(portfolio, position)
		if (success) rm(position)
	}
	else {
		if (!vport)
			cat("Invalid portfolio", portfolio, "\n")
		if (!vpos)
			cat("Invalid position", position, "\n")
		cat("Could not remove", position@name, "from portfolio\n")
		success <- FALSE
	}
	success
}

splatr.valuateportfolio <- function(p, date = date())
{
	positions <- splatr.getpositions(p)
	npos <- length(positions)
	vpos <- numeric(npos)
	value <- p@cash
	for (i in seq(along = positions))
	{
		pos <- positions[i]
		vpos[i] <- splatr.valuateposition(pos, date)
		value <- value + vpos[i]
	}
	for (i in seq(along = positions))
		p@weights[i] <- vpos[i] / value
	p@value <- value
	p@profit <- p@value - p@startcap
	p@return <- splatr.pchange(p@value, p@startcap)
	p@value
}

splatr.closeposition <-
	function(portfolio,
			  position,
			  date = date())
{
	tradesize <- -position@quantity
	position@enddate <- date
	cp <- position@pdata[date == date, close]
	newtrade <- new("splatr.trade",
					  position@name,
					  tradesize,
					  cp,
					  date)
	if (newtrade)
	{
		pmsave <- portfolio@pmatch
		portfolio@pmatch <- FALSE	# force the closing of the position
		status <- splatr.committrade(portfolio,
										 position,
										 newtrade,
										 tradesize,
										 date)
		portfolio@pmatch <- pmsave
	}
	else
	{
		cat("Could not create a trade to close the position\n")
		status <- FALSE
		
	}
	status
}

splatr.showportfolio <-
	function(portfolio, date)
{
	pfstates <- portfolio@gstates
	postates <- portfolio@mstates
	if (missing(date))
		date <- date()
	cat("Portfolio", portfolio@name, "as of", date, "\n")
	value <- portfolio@value
	cash <- portfolio@cash
	ivalue <- value - cash
	profit <- portfolio@profit
	return <- portfolio@return
	cat("   Total value: ", value, "\n")
	cat("   Cash       : ", cash, " (", splatr.percent(cash, value), " %)\n")
	cat("   Investment : ", ivalue, " (", splatr.percent(ivalue, value), " %)\n")
	cat("   Profit     : ", profit, "\n")
	cat("   Return     : ", return, "\n")
	cat("\n")
	# now print current positions
	posubset <- postates[date <= date, ]
	key <- substitute(x$y, list(x = as.character(pfstates), y = pfstates@key))
	posnames <- unique(key)
	positions <- sapply(1:length(posnames),
						  function (i) splatr.getstate(posubset, posnames[i], date))
	positions <- positions[status != "closed"]
	cat("Positions as of", date, "\n\n")
	cat("Name     Status Qty    Basis    Price    Value        Profit     Return  Weight")
	cat("\n")
	fpos <- c(8, 6, 6, 8, 8, 12, 10, 7, 6)
	nfields <- length(fpos)
	spaces <- "            "
	for (i in seq(along = positions))
	{
		position <- positions[i]
		fields[1] <- position$name
		fields[2] <- position$status
		fields[3] <- position$quantity
		fields[4] <- format(round(position$costbasis, 2))
		fields[5] <- format(round(position$price, 2))
		fields[6] <- format(round(position$value, 2))
		fields[7] <- format(round(position$profit, 2))
		fields[8] <- format(round(position$return, 1))
		fields[9] <- format(round(portfolio@weights[i]))
		sapply(1:nfields, function(i)
							 fields[i] <- c(fields[i], substr(spaces, 1, fpos - length(fields[i]))))
		cat(format(fields), "\n", sep = " ")
	}
}

splatr.portfoliosummary <-
	function(portfolio,
			  systems,
			  startdate,
			  enddate,
			  fractal)
{
	if (!missing(systems))
		portfolio <- splatr.run(systems, portfolio, fractal)
	gframe <- splatr.newframe("position", "states", fractal)
	psf <- splatr.getframe(portfolio, gframe)
	psf <- psf[psf$date >= startdate & psf$date <= enddate, ]
	psflength <- nrow(psf)
	psfnew <- psf[psf$status == "new", ]	# get position frame subsets
	psfclosed <- psf[psf$status == "closed", ]
	psflong <- psf[psfnew$quantity > 0, ]
	psfshort <- psf[psfnew$quantity < 0, ]
	psfprofit <- psf[psfclosed$profit > 0, ]
	psfloss <- psf[psfclosed$profit <= 0, ]
	pstate <- splatr.summary.states			# calculate performance
	pstate$totaltrades <- psflength
	pstate$longtrades <- nrow(psflong)
	pstate$shorttrades <- nrow(psfshort)
	pstate$longshortratio <- pstate$longtrades / pstate$shorttrades
	pstate$winners <- nrow(psfprofit)
	pstate$losers <- nrow(psfloss)
	pstate$maxwin <- max(psfprofit$profit)
	pstate$maxloss <- min(psfloss$profit)
	pstate$winpct <- pstate$winners / pstate$totaltrades
	pstate$losepct <- pstate$losers / pstate$totaltrades
	pstate$grossprofit <- sum(psfprofit$profit)
	pstate$grossloss <- sum(psfloss$profit)
	pstate$profitfactor <- pstate$grossprofit / pstate$grossloss
	totalreturn <- cumprod(1 + psfclosed$return)
	pstate$totalreturn <- totalreturn[nrow(psfclosed)]
	#pstate$car <-
	pstate$avgtrade <- mean(psfclosed$return)
	pstate$avgwin <- mean(psfprofit$return)
	pstate$avgloss <- mean(psfloss$return)
	pstate$sdtrade <- sd(psf$return)
	pstate$avgmfe <- mean(psfclosed$mfe)
	pstate$avgmae <- mean(psfclosed$mae)
	pstate$optimalf <- (pstate$winpct * pstate$avgwin + pstate$losepct * pstate$avgloss) / pstate$avgtrade
	runs <- rle(psfclosed$profit > 0)		# create a run of logical 1's and 0's
	cumruns <- cumsum(runs[[1]])
	pstate$totalruns <- length(runs[[2]])
	pstate$longruns <- sum(runs[[2]] > 0)
	pstate$shortruns <- sum(runs[[2]] == 0)
	creturn <- numeric(pstate$totalruns)
	totalprun <- 0
	totallrun <- 0
	maxprun <- 0
	maxlrun <- 0
	maxrunup <- 0
	maxrundown <- 0.0
	for (i in seq(along = pstate$totalruns))
	{
		rlength <- runs[[1]][i]
		rvalue <- runs[[2]][i]
		clength <- cumruns[i]
		creturn[i] <- cumprod(1 + psfclosed$return[(clength-rlength+1):clength])
		if (rvalue > 0) {
			totalprun <- totalprun + rlength
			if (creturn[i] > maxrunup)
				maxrunup <- creturn[i]
			if (rlength > maxprun)
				maxprun <- rlength
		}
		else {
			totallrun <- totallrun + rlength
			if (creturn[i] < maxrundown)
				maxrundown <- creturn[i]
			if (rlength > maxlrun)
				maxlrun <- rlength
		} # end rvalue
	} # end runs loop
	pstate$maxlongrun <- maxprun
	pstate$maxshortrun <- maxlrun
	pstate$avgrun <- pstate$totaltrades / pstate$totalruns
	pstate$avglongrun <- totalprun / pstate$longruns
	pstate$avgshortrun <- totallrun / pstate$shortruns
	pstate$maxrunup <- maxrunup
	pstate$maxrundown <- maxrundown
	pstate$avgrunreturn <- mean(creturn)
	pstate$avgrunup <- mean(creturn[creturn > 1])
	pstate$avgrundown <- mean(creturn[creturn <= 1])
	pstate$tradingperiod <- difftime(psf$enddate[psflength] - psf$startdate[i])
	pstate$totalcoverage <- sum(psfclosed$duration) / pstate$tradingperiod
	pstate$longcoverage <- sum(psflong$duration) / pstate$tradingperiod
	pstate$shortcoverage <- sum(psfshort$duration) / pstate$tradingperiod
	pstate$frequency <- pstate$tradingperiod / pstate$totaltrades
	pstate$longfrequency <- pstate$tradingperiod / pstate$longtrades
	pstate$shortfrequency <- pstate$tradingperiod / pstate$shorttrades
}

splatr.depositportfolio <-
	function(p,
			  cash,
			  date = date())
{
	p@cash <- p@cash + cash
	splatr.valuateportfolio(p, date)
}

splatr.withdrawportfolio <-
	function(p,
			  cash,
			  date = date())
{
	currentcash <- p@cash
	availcash <- currentcash - (p@mincash * p@value)
	if (cash > availcash)
	{
		cat("Withdrawal of", cash, "would exceed reserve amount\n")
		p@value
	}
	else
	{
		p@cash <- currentcash - cash
		splatr.valuateportfolio(p, date)
	}
}

splatr.deleteportfolio <-
	function(portfolio)
{
	positions <- splatr.getpositions(portfolio)
	npos <- length(positions)
	sapply(1:npos, function (i) splatr.closeposition(portfolio, positions[i]))
	rm(portfolio)
}