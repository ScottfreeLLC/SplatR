##############################################################
#
# Package   : splatr
# Module    : global.states
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

# Football

splatr.football.states <<- list(
	osma = 0.0,
	dsma = 0.0,
	olma = 0.0,
	dlma = 0.0,
	tsma = 0.0,
	tlma = 0.0,
	omadelta = 0.0,
	dmadelta = 0.0,
	madelta = 0.0)	# more to add
	
# Portfolio

splatr.portfolio.states <<- list(
	cash = 0.0,
	value = 0.0,
	profit = 0.0,
   netreturn = 0.0,
	numpos = 0,
	leverage = 0.0,
	runup = 0.0,
	drawdown = 0.0)   # more to add
	
# Position
							  
splatr.position.states <<- list(
	name="",
	quantity = 0,
	price = 0.0,
	status = "na",	# "new", "open", "closed"
	value = 0.0,
	profit = 0.0,
	costbasis = 0.0,
	netreturn = 0.0,
	startdate = date(),
	enddate = date(),
	duration = 0.0, 	# calculate when updating position
	mfe = 0.0,
	mae = 0.0)
	
# Performance Summary

splatr.summary.states <<- list(
	totaltrades = 0,		# trade statistics
	tradingperiod = 0,
	winners = 0,
	losers = 0,
	winnerpct = 0.0,
	loserpct = 0.0,
	avgwin = 0.0,
	avgloss = 0.0,
	avgtrade = 0.0,
	sdtrade = 0.0,
	maxwin = 0.0,
	maxloss = 0.0,
	avgmfe = 0.0,
	avgmae = 0.0,
	sdmfe = 0.0,
	sdmae = 0.0,
	grossprofit = 0.0,	# portfolio statistics
	grossloss = 0.0,
	profitfactor = 0.0,
	twr = 0.0,
	car = 0.0,
	optimalf = 0.0,
	maxrunup = 0.0,
	maxdrawdown = 0.0,
	totalruns = 0,
	longruns = 0,
	shortruns = 0,
	avgrun = 0,
	avgrunup = 0.0,
	avgdrawdown = 0.0,
	avglongrun = 0,
	avgshortrun = 0,
	maxlongrun = 0,
	maxshortrun = 0,
	totalcoverage = 0.0,
	longcoverage = 0.0,
	shortcoverage = 0.0,
	frequency = 0.0,
	longfrequency = 0.0,
	shortfrequency = 0.0)
	
# System

splatr.system.states <<- list(
	letrigger = FALSE,
	leprice = 0.0,
	lxtrigger = FALSE,
	lxprice = 0.0,
	setrigger = FALSE,
	seprice = 0.0,
	sxtrigger = FALSE,
	sxprice = 0.0,
	marketposition = "flat",
	tradestate = "",
	tradenumber = 0,
	tradepl = 0.0,
	longpl = 0.0,
	shortpl = 0.0,
	totalpl = 0.0)	# add mfe = 0.0, mae = 0.0
	
# Trade
	
splatr.trade.states <<- list(
	date = date(),
	name = "",
	quantity = 0,
	price = 0.0)
