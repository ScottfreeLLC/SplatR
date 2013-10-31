##############################################################
#
# Package   : splatr
# Module    : initialize
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

#
# Initialize statistics
#
# "t value", "z value", "Std. Error", "Estimate", "AIC", "DF", "Deviance",
# "R^2", "bias", "F value", "BIC", "logLik", "Std Dev", "Corr"
#

splatr.stat.pcuts <- c(0.0, 0.001, 0.01, 0.05, 0.1, 1.0)
splatr.stat.plabels <- c("***", "**", "*", ".", "-")
splatr.stat.pvalue <- new("splatr.statistic",
                          sname = "p-value",
                          rname = "Pr(>|z|)",
                          desc = "sig stars",
                          cuts = splatr.stat.pcuts,
                          cutlabels = splatr.stat.plabels,
                          outputfn = splatr.getstat)

splatr.stat.zranges <- seq(from = -3.0, to = 3.0, by = 0.5)
splatr.stat.zvalue <- new("splatr.statistic",
                          sname = "z-value",
                          rname = "z value",
                          desc = "z scores",
                          cuts = splatr.stat.zranges,
                          cutlabels = as.character(splatr.stat.zranges),
                          outputfn = splatr.getstat)

#
# Define the methods
#

# Logistic Regression

splatr.lrmethod <<- splatr.newmethod(ifname = "glm",
                                     ifopt = "binomial",
                                     ofname = "predict",
                                     ofopt = "type=\"response\"",
                                     stats = list(splatr.stat.pvalue,
                                                  splatr.stat.zvalue))

# Multiple Regression

splatr.mrmethod <<- splatr.newmethod(ifname = "lm",
                                     ifopt = "",
                                     ofname = "predict",
                                     ofopt = "",
                                     stats = list(splatr.stat.pvalue,
                                                  splatr.stat.zvalue))


# Multinomial

splatr.mnmethod <<- splatr.newmethod(ifname = "multinom",
                                     ifopt = "",
                                     ofname = "predict",
                                     ofopt = "type=\"class\"",
                                     stats = list(splatr.stat.pvalue,
                                                  splatr.stat.zvalue))
# Classification Trees

splatr.ctmethod <<- splatr.newmethod(ifname = "rpart",
                                     ifopt = "",
                                     ofname = "predict",
                                     ofopt = "type=\"class\"",
                                     stats = list())

# Random Forests

splatr.rfmethod <<- splatr.newmethod(ifname = "randomForest",
                                     ifopt = "importance=TRUE",
                                     ofname = "predict",
                                     ofopt = "",
                                     stats = list())
#
# Define feeds
#

splatr.newfeed("yahoo", 
               "stock",
               "http://ichart.finance.yahoo.com",
               ",")

# splatr.newfeed

# define external directories

# Nasdaq Stocks

#splatr.newdirectory(
#	"nasdaq",
#	"stock",
#	"ftp://ftp.nasdaqtrader.com/symboldirectory/nasdaqlisted.txt",
#	"|",
#	"Symbol")
	 
# NYSE Stocks

#splatr.newdirectory(
#	"nyse",
#	"stock",
#	"ftp://ftp.nasdaqtrader.com/symboldirectory/otherlisted.txt",
#	"|",
#	"ACT.Symbol")
	 
# PBOT Futures

#splatr.newdirectory(
#	"pbot",
#	"future",
#	"ftp://ftp.nasdaqtrader.com/symboldirectory/pbot.csv",
#	"|",
#	"Symbol")

# All Mutual Funds

#splatr.newdirectory(
#	"all",
#	"mutualfund",
#	"ftp://ftp.nasdaqtrader.com/symboldirectory/mfundslist.txt",
#	"|",
#	"Fund.Symbol")

# Nasdaq Options

#splatr.newdirectory (
#	"nasdaq",
#	"option",
#	"ftp://ftp.nasdaqtrader.com/symboldirectory/options.txt",
#	"|",
#	"Symbol")
