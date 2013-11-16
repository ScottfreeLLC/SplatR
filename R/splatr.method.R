##############################################################
#
# Package   : splatr
# Module    : method
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

# Functions

splatr.newmethod <-
   function(ifname = "glm",
            ifopt = "binomial",
            ofname = "predict",
            ofopt = "",
            stats = splatr.stat.pvalue)
{
   newm <- new("splatr.method",
               ifname = ifname,
               ifopt = ifopt,
               ofname = ofname,
               ofopt = ofopt,
               stats = stats)
   newm
}

# Called by the analysis object

splatr.runmethod <- function(sname, groupby, fitfn)
{
   # omit NA cases
   sdata <- na.omit(get(sname))
   # run the analysis
   suppressWarnings(models <- dlply(sdata, .(get(groupby)), fitfn))
   models
}
