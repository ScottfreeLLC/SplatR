splatr.predict <-
	function(group,
			  responses = "return",
			  variables = ".",
			  leadvars = "",
			  method = new("splatr.mr.method"),
			  lag = 1,
			  groupby = c("month", "year"),
			  dframe = splatr.newframe("price", "states", "d1"))
{
	splatr.setgroupframe(group, dframe)
	for (i in seq(along=responses))
	{
		response <- responses[i]
		aframe <- splatr.newframe(response, "splatr", dframe@fractal)
		p <- splatr.newanalysis(group,
						  		   response,
						  		   variables,
						  		   leadvars,
						  		   method,
						  		   lag,
						  		   dframe@fid,
						  		   dframe@fractal,
						  		   groupby)
		if (nexists(p))
			cat("splatr prediction:", splatr.getname(p), "is complete\n")
		else
			stop("splatr prediction did not complete\n")
		rm(aframe)
	}
}