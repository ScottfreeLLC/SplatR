##############################################################
#
# Package   : splatr
# Module    : sboost
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.sboost <-
	function(group,
			   system,
			   fractal = "d1",
			   outputs = "return",
			   inputs = ".",
			   lag = 1,
			   groupby = c("month", "year"),
			   ...)
{
	gspace <- splatr.newspace("systems", "trades", fractal)
   group$space <- gspace
	alltrades <- splatr.gapply(group, splatr.mergeFrames, system)
	portfolio <- splatr.generateportfolio(group, alltrades, 0, restricted = FALSE, ...)
	gspace$what <- "position"
	gspace$wherein <- "states"
   group$space <- gspace   # now we move to position states
	splatr.splitframe(group)
	aspace <- splatr.newspace(system, "sboost", fractal)
	method <- new("splatr.mrmethod")
	for (i in seq(along = outputs))
	{
		response <- responses[i]
		sb <- splatr.predict(group,
                           output[i],
                           inputs,
                           method,
                           lag,
                           groupby,
								   aspace)
		if (exists(sb))
			cat("System Boost", splatr.getname(sb), "is complete\n")
		else
			cat("System Boost for", response, "did not complete\n")
	}
	rm(gspace, aspace)
}