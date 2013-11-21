##############################################################
#
# Package   : splatr
# Module    : load
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

source('~/SplatR/R/splatr.R')
source('~/SplatR/R/splatr.utility.R')
source('~/SplatR/R/splatr.global.states.R')
source('~/SplatR/R/splatr.variables.R')
source('~/SplatR/R/yahoo.global.states.R')
source('~/SplatR/R/splatr.space.R')
source('~/SplatR/R/splatr.frame.R')
source('~/SplatR/R/splatr.states.R')
source('~/SplatR/R/splatr.group.R')
source('~/SplatR/R/splatr.classes.R')
source('~/SplatR/R/splatr.statistic.R')
source('~/SplatR/R/splatr.fractal.R')
source('~/SplatR/R/splatr.directory.R')
source('~/SplatR/R/splatr.feed.R')
source('~/SplatR/R/splatr.feed.yahoo.R')
source('~/SplatR/R/splatr.method.R')
source('~/SplatR/R/splatr.name.R')
source('~/SplatR/R/splatr.variable.R')
source('~/SplatR/R/splatr.variable.functions.R')
source('~/SplatR/R/splatr.system.R')
source('~/SplatR/R/splatr.trade.R')
source('~/SplatR/R/splatr.analysis.R')
source('~/SplatR/R/splatr.position.R')
source('~/SplatR/R/splatr.portfolio.R')
source('~/SplatR/R/splatr.predict.R')
source('~/SplatR/R/splatr.game.R')
source('~/SplatR/R/splatr.pboost.R')
source('~/SplatR/R/splatr.sboost.R')
source('~/SplatR/R/splatr.initialize.R')
#splatr.newgroup("tech", members=c("aapl", "amzn", "goog", "lnkd", "qqq", "spy"))
splatr.newgroup("tech", members=c("aapl"))
tech.stock$members
splatr.getdata(tech.stock, yahoo.stock.price.feed.d1)
tech.space <- splatr.newspace()
tech.space
tech.inputs <- c("month", "quarter", "year", "weekday", "higherclose", "lowerclose", "gapup", "gapdown", "higherhigh", "lowerlow", "lowerhigh", "higherlow", "higheropen", "loweropen", "bagapup", "bagapdown", "truerange", "atr", "vma", "atr20", "sepoc", "sepol", "sepoh", "sepcl", "sepch", "highsep", "net", "net2", "atr20", "netreturn", "netreturn2", "netreturn3", "netreturn4", "netreturn5", "volratio", "sepoc2", "sepoc3", "sepoc4", "bigup", "bigdown")
tech.inputs
splatr.vapply(tech.stock, tech.space, tech.inputs)
head(aapl.stock.price.states.d1)
#splatr.newanalysis(tech.stock, inputs=tech.inputs, method=splatr.rfmethod, groupby="quarter")
#importance(tech.stock.highsep.analysis.d1$models[[1]])
#varImpPlot(tech.stock.highsep.analysis.d1$models[[1]])
#splatr.newsystem("closer", longevent = "higherclose", shortevent = "lowerclose")
#splatr.runsystem(tech.stock, "closer")
splatr.newsystem("big", longevent = "bigup", shortevent = "bigdown")
#splatr.runsystem(tech.stock, "big")