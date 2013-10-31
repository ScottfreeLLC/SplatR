##############################################################
#
# Package   : splatr
# Module    : classes
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

require(chron)

setClass("splatr.statistic",
	representation(
		sname = "character",
      rname = "character",
      desc = "character",
		cuts = "numeric",
		cutlabels = "character",
      outputfn = "function"),
	validity = splatr.validstat)

setClass("splatr.directory",
	contains = "splatr",
	representation(
		uri = "character",
		sep = "character",
		key = "character",
		frame = "data.frame"),
	validity = splatr.validdirectory
)

setOldClass("dates")

setClass("splatr.feed",
	contains = "splatr",
	representation(
		uri = "character",
		sep = "character",
		dir = "character",
		startdate = "dates",
		enddate = "dates",
		fframe = "splatr.frame",
		getfn = "character",
		gotfn = "character"),
	validity = splatr.validfeed
)

setClass("splatr.method",
	representation(
		ifname = "character",
		ifopt = "character",
		ofname = "character",
      ofopt = "character",
      stats = "list"),
   validity = splatr.validmethod)

setClassUnion("splatr.name", c("splatr.group", "character"))

setClass("splatr.trade",
	representation(date = "character",
					 quantity = "numeric",
					 name = "character",
					 price = "numeric",
					 qoffset = "numeric"),
	validity = splatr.validtrade)

setClass("splatr.position",
	representation(
		trades = "splatr.group",
		quantity = "numeric",
		price = "numeric",
		startdate = "character",
		enddate = "character",
		status = "character",	# "new", "open", or "closed"
		pdata = "splatr.states",
		mdata = "list",
		value = "numeric",
		profit = "numeric",
		costbasis = "numeric",
		nreturn = "numeric"),
	validity = splatr.validposition)

setClass("splatr.portfolio",
	representation(
		positions = "splatr.group",
		cash = "numeric",			# state
		margin = "numeric",
		mincash = "numeric",
		fixedfrac = "numeric",
		maxpos = "numeric",
		posby = "character",
		maxloss = "numeric",
		kopos = "numeric",
		koby = "character",
		restricted = "logical",	# default TRUE
		pmatch = "logical",		# exact TRUE, any FALSE
		psearch = "logical",		# fifo TRUE, lifo FALSE
		weightby = "character",
		weights = "numeric",
		startdate = "character",
		startcap = "numeric",
		value = "numeric",
		profit = "numeric",
		nreturn = "numeric"),
	validity = splatr.validportfolio)

setRefClass("splatr.analysis",
   fields = list(
      name = "character",
      group = "splatr.group",
      response = "character",
      variables = "character",
      method = "splatr.method",
      lag = "numeric",
      aframe = "splatr.frame",
      groupby = "character",
      models = "list",
      results = "data.frame",
      cvparams = "numeric",
      cverror = "numeric",
      wfparams = "numeric",
      wferror = "numeric"))

setClass("splatr.system",
	contains = "splatr",
	representation(
		leevent = "character",
		leorder = "list",
		lxevent = "character",
		lxorder = "list",
		seevent = "character",
		seorder = "list",
		sxevent = "character",
		sxorder = "list"))

setClass("splatr.game",
	contains = "splatr",
	representation(group = "splatr.group",
					 gdir = "splatr.directory"))
	
