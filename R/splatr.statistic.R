##############################################################
#
# Package   : splatr
# Module    : statistics
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.fixtable <- function(stable, v)
{
   # check for aliased variables that summary excludes
   nrows <- nrow(stable) - 1
   ncols <- ncol(stable)
   vcount <- length(v)
   if (nrows < vcount) {
      # there are aliased variables
      vsub <- setdiff(rownames(stable), "(Intercept)")
      # remove TRUE from logicals
      vsub <- gsub("TRUE", "", vsub)
      vmiss <- setdiff(v, vsub)
      # adjust vpos by 1 for intercept
      vpos <- match(vmiss, v) + 1
      vmcount <- length(vmiss)
      # allocate new table
      newtable <- matrix(nrow = vcount + 1, ncol = ncols)
      newseq <- setdiff(seq(1:(vcount + 1)), vpos)
      newtable[newseq, ] <- stable[1:(nrows + 1), ]
      newtable
   }
   else
      stable   
}

splatr.getstat <-
   function(stat,
            inputs,
            models)
   {
      svec <- numeric()
      sname <- stat$rname
      nvars <- length(inputs)
      nmodels <- length(models)
      stable <- summary(models[[1]])$coefficients
      snames <- colnames(stable)
      sindex <- match(sname, snames)
      if (!is.na(sindex)) {
         svec <- numeric(nmodels * nvars)
         first <- (sindex - 1) * (nvars + 1) + 2
         last <- first + nvars - 1
         for (i in seq(along = models))
         {
            stable <- summary(models[[i]])$coefficients
            stable <- splatr.fixtable(stable, inputs)
            svec[seq(from = i, by = nmodels, length = nvars)] <- stable[first:last]         
         }
      }
      unlist(svec)
   }


