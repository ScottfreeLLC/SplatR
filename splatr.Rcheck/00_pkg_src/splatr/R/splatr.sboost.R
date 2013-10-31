splatr.sboost <-
	function(group,
			  system,
			  fractal = "d1",
			  responses = "return",
			  variables = ".",
			  lag = 1,
			  groupby = c("month", "year"),
			  ...)
{
	gframe <- splatr.newframe("systems", "trades", fractal)
	splatr.setframe(group, gframe)
	alltrades <- splatr.gapply(group, splatr.mergeFrames, system)
	portfolio <- splatr.generateportfolio(group, alltrades, 0, restricted = FALSE, ...)
	gframe@fdata <- "position"
	grame@fclass <- "states"
	splatr.setframe(group, gframe)		# now we move to position states
	splatr.splitframe(group)
	aframe <- splatr.newframe(system, "sboost", fractal)
	method <- new("splatr.mrmethod")
	for (i in seq(along = responses))
	{
		response <- responses[i]
		sb <- splatr.analyze(group,
								aframe,
								response,
								variables,
								method,
								lag,
								groupby)
		if (exists(sb))
			cat("System Boost", splatr.getname(sb), "is complete\n")
		else
			cat("System Boost for", response, "did not complete\n")
	}
	rm(gframe, aframe, method)
}