##############################################################
#
# Package   : splatr
# Module    : predict
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.predict <-
	function(group,
			   output = "return",
			   inputs = ".",
			   method = splatr.mrmethod,
			   lag = 1,
			   groupby = c("month", "year"),
			   pspace = splatr.newspace("price", "states", "d1"))
{
	group$space <- pspace
	for (i in seq(along=responses))
	{
		response <- responses[i]
		aspace <- splatr.newspace(output, "splatr", pspace$fractal)
		p <- splatr.newanalysis(group,
                              output,
                              inputs,
                              method,
                              lag,
                              groupby,
                              aspace)
		if (nexists(p))
			cat("splatr prediction:", splatr.getname(p), "is complete\n")
		else
			stop("splatr prediction did not complete\n")
		rm(aspace)
	}
   rm(pspace)
}