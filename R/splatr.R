##############################################################
#
# Package   : splatr
# Module    : splatr
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

require(chron)
require(data.table)
require(plyr)
require(randomForest)
require(rpart)

# splatr class definitions

setRefClass("splatr",
	fields = list(
      name = "character",
      subject = "character"),
	contains = "VIRTUAL")

# define R vector functions

splatr.vectorfunctions <<-
   c("max",
     "min",
     "sum",
     "mean",
     "median",
     "range",
     "var",
     "cor",
     "sort",
     "rank",
     "order",
     "quantile",
     "cumsum",
     "cumprod",
     "cummax",
     "cummin",
     "pmax",
     "pmin",
     "colMeans",
     "colSums",
     "rowMeans",
     "rowSums")
