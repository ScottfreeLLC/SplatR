##############################################################
#
# Package   : splatr
# Module    : classes
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

setRefClass("splatr.statistic",
	fields = list(
		sname = "character",
      rname = "character",
      desc = "character",
		cuts = "numeric",
		cutlabels = "character",
      outputfn = "function"))

setRefClass("splatr.directory",
	contains = "splatr",
	fields = list(
		uri = "character",
		sep = "character",
		key = "character",
		frame = "data.table"))

setOldClass("dates")
setOldClass("times")

setRefClass("splatr.feed",
	contains = "splatr",
	fields = list(
		uri = "character",
		sep = "character",
      key = "character",
		dir = "character",
		startdate = "dates",
		enddate = "dates",
		space = "splatr.space",
		getfn = "character",
		gotfn = "character"))

setRefClass("splatr.method",
	fields = list(
		ifname = "character",
		ifopt = "character",
		ofname = "character",
      ofopt = "character",
      stats = "list"))

setRefClass("splatr.trade",
	fields = list(
      name = "character",
      quantity = "numeric",
      price = "numeric",
      tdate = "dates"))

setRefClass("splatr.position",
	fields = list(
      name = "character",
	   startdate = "dates",
	   enddate = "dates",
	   duration = "times",
      quantity = "numeric",
      status = "character",	# "flat", "long", or "short"
	   price = "numeric",
 		trades = "list",
		ntrades = "numeric",
		pname = "character",
		multiplier = "numeric",
		value = "numeric",
		profit = "numeric",
		costbasis = "numeric",
		netreturn = "numeric",
      mae = "numeric",
      mfe = "numeric"))

setRefClass("splatr.portfolio",
   contains = "splatr.group",
	fields = list(
	   startdate = "dates",
	   enddate = "dates",
      duration = "times",
	   npos = "numeric",       # position fields
	   maxpos = "numeric",
	   posby = "character",
	   kopos = "numeric",
	   koby = "character",
	   restricted = "logical",	# default TRUE
	   weightby = "character",
	   weights = "numeric",
	   startcap = "numeric",   # state
	   cash = "numeric",
		margin = "numeric",
		mincash = "numeric",
		fixedfrac = "numeric",
		maxloss = "numeric",
		value = "numeric",
		profit = "numeric",
      runup = "numeric",
      drawdown = "numeric",
		netreturn = "numeric",
      gsname = "character",
      msname = "character"))

setRefClass("splatr.analysis",
   fields = list(
      name = "character",
      group = "splatr.group",
      output = "character",
      inputs = "character",
      method = "splatr.method",
      lag = "numeric",
      space = "splatr.space",
      groupby = "character",
      models = "list",
      results = "data.table",
      cvparams = "numeric",
      cverror = "numeric",
      wfparams = "numeric",
      wferror = "numeric"))

setRefClass("splatr.system",
	contains = "splatr",
	fields = list(
		longevent = "character",
		shortevent = "character",
		holdperiod = "numeric",
      scale = "logical"))

setRefClass("splatr.game",
	contains = "splatr",
	fields = list(
      group = "splatr.group",
      gdir = "splatr.directory"))
	