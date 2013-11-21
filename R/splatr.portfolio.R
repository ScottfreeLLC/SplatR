##############################################################
#
# Package   : splatr
# Module    : portfolio
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getportfolioname <- function(name, subject)
{
   pname <- paste(name, subject, "portfolio", sep=".")
   pname
}

splatr.setportfolio <- function(pname, pobject)
{
   assign(pname, pobject, envir = .GlobalEnv)
   pname
}

splatr.getportfolio <- function(pname)
{
   splatr.getref(pname)
}

splatr.newportfolio <-
   function(name = "group",
            subject = "stock",
            space = splatr.newspace("price", "states", "d1"),
            startdate = splatr.getdate(),
            date = splatr.getdate(),
            maxpos = 10,
            posby = "",
            kopos = 0,
            koby = "-profit",
            restricted = FALSE,
            weightby = "quantity",
            weights = 0,
            startcap = 100000,
            margin = 0.5,
            mincash = 0.2,
            fixedfrac = 0.1,
            maxloss = 0.1)
{
   # initialize group attributes
   dynamic <- TRUE
   recursive <- TRUE
   gstate <- "portfolio"
   mstate <- "position"
   members <- as.character(c())
   # initialize portfolio attributes
   duration <- date - startdate
   npos <- 0
   cash <- startcap
   value <- startcap
   profit <- 0.0
   runup <- 0.0
   drawdown <- 0.0
   netreturn <- 0.0
   # find the portfolio
   pname <- splatr.getportfolioname(name, subject)
   pnew <- splatr.getportfolio(pname)
   if (is.null(pnew))
   {
      # set up states objects
      gspace <- splatr.newspace(gstate, "states", space$fractal)
      gsname <- splatr.setgroupstates(name, subject, gspace)
      mspace <- splatr.newspace(mstate, "states", space$fractal)
      msname <- splatr.setgroupstates(name, subject, mspace)
      # create portfolio object
      pnew <- new("splatr.portfolio",
                  name = name,            # group fields
                  subject = subject,
                  dynamic = dynamic,
                  recursive = recursive,
                  members = members,
                  gstate = gstate,
                  mstate = mstate,
                  space = space,
                  startdate = startdate,  # portfolio fields
                  date = date,
                  duration = duration,
                  npos = npos,            # position fields
                  maxpos = maxpos,
                  posby = posby,
                  kopos = kopos,
                  koby = koby,
                  restricted = restricted,
                  weightby = weightby,
                  weights = weights,
                  startcap = startcap,
                  cash = cash,
                  margin = margin,
                  mincash = mincash,
                  fixedfrac = fixedfrac,
                  maxloss = maxloss,
                  value = value,
                  profit = profit,
                  runup = runup,
                  drawdown = drawdown,
                  netreturn = netreturn,
                  gsname = gsname,
                  msname = msname)
      if (!is.null(pnew)) {
         # set portfolio name
         splatr.setportfolio(pname, pnew)
         # load members from fixed groups
         if (!dynamic) {
            dname <- splatr.getdirectoryname(name, subject)
            dir <- splatr.getdirectory(dname)
            members <- with(dir, dir$key)
         }
      }
   }
   else
      cat("Portfolio", pname, "already exists\n")
   rm(space)
   pname
}

splatr.generateportfolio <-
	function(group,
            tfname,
			   startcap,
            posby,
            bperiod)
{
   pname <- splatr.newportfolio(group$name,
	                             group$subject,
                                startcap = startcap,
                                posby = posby,
	                             restricted = FALSE)
	splatr.maketrades(pname,
                     tfname,
                     bperiod)
   pname
}

splatr.addportfolio <- function(portfolio, pname)
{
   splatr.addgroup(portfolio, pname)
}

splatr.removeportfolio <- function(portfolio, pname)
{
   splatr.removegroup(portfolio, pname, remove = TRUE)
}

splatr.updateportfolio <-
   function(portfolio,
            position,
            trade,
            allocation)
{
   # update position
   ppq <- abs(position$quantity)
   splatr.updateposition(portfolio, position, trade)
   cpq <- abs(position$quantity)
   npq <- cpq - ppq
   # update portfolio
   portfolio$date <- trade$tdate
   multiplier <- position$multiplier
   cv <- trade$price * multiplier * npq
   portfolio$cash <- portfolio$cash - cv
   value <- splatr.valuateportfolio(portfolio, trade$tdate)
   # record the portfolio and position states
   splatr.setstate(portfolio$gsname, portfolio)
   splatr.setstate(portfolio$msname, position)
   # if net position is zero, then close the position
   if (position$quantity == 0)
      splatr.removeportfolio(portfolio, position$pname)
}

splatr.balance <-
	function(portfolio,
			   tdate,
			   cashlevel)
{
   stopifnot(!portfolio$pmatch)
	currentcash <- portfolio$cash
	mincash <- portfolio$mincash
	weightby <- portfolio$weightby
	if (missing(cashlevel))
		cashlevel <- mincash
	value <- splatr.valuateportfolio(portfolio, tdate)
	pvalue <- value - cashlevel * value
	positions <- portfolio$members
	npos <- length(positions)
	bdata <- numeric(npos)
	if (weightby != "")
   {
		plusminus <- substr(weightby, 1, 1)
		invert <- ifelse(plusminus == "-", TRUE, FALSE)
		variable <- sub("-", "", weightby)
		wbpos <- match(weightby, names(getRefClass("splatr.position")$fields()))
		if (!is.na(wbpos))
			bdata <- sapply(1:npos,
							      function (i) eval(substitute(positions[i]$x, x=variable)))
		else
			bdata <- sapply(1:npos,
							    function (i)
                         {
                            position <- positions[i]
                            pname <- position$pname
                            pdata <- splatr.getref(pname)
                            splatr.getstate(pdata, "date", tdate, variable)
							    })
		if (invert)
			bweights <- (2 * mean(bdata) - bdata) / sum(bdata)
		else
			bweights <- bdata / sum(bdata)
	}
	for (i in seq(along = positions))
	{
		position <- positions[i]
		multiplier <- position$multiplier
		pname <- position$pname
		pdata <- splatr.getref(pname)
		bdelta <- bweights[i] * pvalue - position$value
		cp <- splatr.getstate(pdata, tdate, "close")
		tradesize <- trunc(bdelta / cp)
		ntv <- abs(tradesize) * cp * multiplier
		splatr.committrade(
               portfolio,
					position,
					splatr.newtrade(position$name, tradesize, cp, tdate),
					0)
		portfolio$cash <- currentcash + bdelta - ntv
	}
}

splatr.kickout <-
	function(portfolio,
		      tdate,
			   freepos)
{
	positions <- portfolio$members
	npos <- length(positions)
	kodata <- numeric(npos)
   koby <- portfolio$koby
	koby <- ifelse(koby == "", "profit", koby)
	plusminus <- substr(koby, 1, 1)
	descending <- ifelse(plusminus == "-", TRUE, FALSE)
	variable <- sub("-", "", koby)
   kpos <- match(koby, names(getRefClass("splatr.position")$fields()))
	if (!is.na(kpos))
		kodata <- sapply(1:npos,
							function (i) eval(parse(text=paste("positions[i]", koby, sep="$"))))
	else
		kodata <- sapply(1:npos,
                       function (i)
                       {
                          position <- positions[i]
                          pname <- position$pname
                          pdata <- splatr.getref(pname)
                          splatr.getstate(pdata, tdate, variable)
                       })
	koorder <- ifelse(descending, rev(order(kodata)), order(kodata))
	if (missing(freepos)) {
		kopos <- portfolio$kopos
		maxpos <- portfolio$maxpos
		opos <- maxpos - npos
		freepos <- ifelse(opos <= 0, kopos - opos, 0)
	}
	closed <- sapply(1:freepos,
					     function(i) splatr.closeposition(portfolio,
                                                     positions[koorder[i]],
                                                     tdate))
}

splatr.stoploss <- function(portfolio, tdate)
{
	positions <- portfolio$members
	npos <- length(positions)
	maxloss <- portfolio$maxloss
	mldata <- sapply(1:npos, function(i) positions[i]$return)
	mlpos <- which(mldata <= -maxloss)
	closed <- sapply(1:length(mlpos),
					     function (i) splatr.closeposition(portfolio,
                                                      positions[mlpos[i]],
                                                      tdate))
}

splatr.valuateportfolio <- function(p, tdate)
{
	positions <- p$members
	npos <- length(positions)
	vpos <- numeric(npos)
	value <- p$cash
	for (i in seq(along = positions))
	{
		pname <- positions[i]
      pos <- splatr.getposition(pname)
		vpos[i] <- splatr.valuateposition(pos, tdate)
		value <- value + vpos[i]
	}
	for (i in seq(along = positions))
	   p$weights[i] <- vpos[i] / value
	p$value <- value
	p$profit <- p$value - p$startcap
	p$netreturn <- pchange(p$value, p$startcap)
	p$value
}

splatr.showportfolio <-
	function(portfolio, tdate)
{
	pfstates <- portfolio$gstates
	postates <- portfolio$mstates
	if (missing(tdate))
		tdate <- date()
	cat("Portfolio", portfolio$name, "as of", tdate, "\n")
	value <- portfolio$value
	cash <- portfolio$cash
	ivalue <- value - cash
	profit <- portfolio$profit
	netreturn <- portfolio$netreturn
	cat("   Total value: ", value, "\n")
	cat("   Cash       : ", cash, " (", percent(cash, value), " %)\n")
	cat("   Investment : ", ivalue, " (", percent(ivalue, value), " %)\n")
	cat("   Profit     : ", profit, "\n")
	cat("   Return     : ", netreturn, "\n")
	cat("\n")
	# now print current positions
	posubset <- postates[date <= tdate, ]
	key <- substitute(x$y, list(x = as.character(pfstates), y = pfstates$key))
	posnames <- unique(key)
	positions <- sapply(1:length(posnames),
						  function (i) splatr.getstate(posubset, posnames[i], tdate))
	cat("Positions as of", tdate, "\n\n")
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
		fields[8] <- format(round(position$netreturn, 1))
		fields[9] <- format(round(portfolio$weights[i]))
		sapply(1:nfields, function(i)
							 fields[i] <- c(fields[i], substr(spaces, 1, fpos - length(fields[i]))))
		cat(format(fields), "\n", sep = " ")
	}
}

splatr.portfoliosummary <-
	function(portfolio,
			   system,
			   startdate,
			   enddate,
			   fractal)
{
	if (!missing(system))
		portfolio <- splatr.runsystem(system, portfolio, fractal)
	gspace <- splatr.newspace("position", "states", fractal)
	psf <- splatr.getframe(portfolio, gspace)
	psf <- psf[psf$date >= startdate & psf$date <= enddate, ]
	psflength <- nrow(psf)
	psflong <- psf[psf$quantity > 0, ]
	psfshort <- psf[psf$quantity < 0, ]
   psfflat <- psf[psf$quantity == 0, ]
	psfprofit <- psf[psf$profit > 0, ]
	psfloss <- psf[psf$profit <= 0, ]
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
	totalreturn <- cumprod(1 + psfclosed$netreturn)
	pstate$totalreturn <- totalreturn[nrow(psfflat)]
	#pstate$car <-
	pstate$avgtrade <- mean(psfclosed$netreturn)
	pstate$avgwin <- mean(psfprofit$netreturn)
	pstate$avgloss <- mean(psfloss$netreturn)
	pstate$sdtrade <- sd(psf$netreturn)
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
		creturn[i] <- cumprod(1 + psfclosed$netreturn[(clength-rlength+1):clength])
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
			   tdate = splatr.getdate())
{
	p$cash <- p$cash + cash
	splatr.valuateportfolio(p, tdate)
}

splatr.withdrawportfolio <-
	function(p,
			  cash,
			  tdate = splatr.getdate())
{
	currentcash <- p$cash
	availcash <- currentcash - (p$mincash * p$value)
	if (cash > availcash)
	{
		cat("Withdrawal of", cash, "would exceed reserve amount\n")
		p$value
	}
	else
	{
		p$cash <- currentcash - cash
		splatr.valuateportfolio(p, tdate)
	}
}

splatr.deleteportfolio <-
	function(portfolio)
{
	positions <- portfolio$members
	npos <- length(positions)
	sapply(1:npos, function (i) splatr.closeposition(portfolio, positions[i]))
	rm(portfolio)
}