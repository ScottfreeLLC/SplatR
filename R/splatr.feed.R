##############################################################
#
# Package   : splatr
# Module    : feed
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

splatr.newfeed <-
	function(name = "yahoo", 
			   subject = "stock",
			   uri = "",
			   sep = ",",
            key = "date",
			   dir = splatr.getdirectoryname(name, subject),
			   startdate = splatr.getdate(365),
			   enddate = splatr.getdate(),
	         pspace = splatr.newspace("price", "states", "d1"),
			   getfn,
			   gotfn)
{
	# assign functions for getting and processing data
	if (missing(getfn))
		getfn <- paste("splatr.getdata.", name, sep="")
	if (missing(gotfn))
		gotfn <- paste("splatr.gotdata.", name, sep="")
	# create feed object
	nf <- new("splatr.feed",
				 name = name,
             subject = subject,
             uri = uri,
             sep = sep,
             key = key,
             dir = dir,
             startdate = startdate,
             enddate = enddate,
             space = pspace,
             getfn = getfn,
             gotfn = gotfn)
   if (!is.null(nf)) {
      fspace <- splatr.newspace(pspace$what, "feed", pspace$fractal)
      fname <- splatr.getframename(name, subject, fspace)
      splatr.setframe(fname, nf)
      rm(fspace)
   }
}

splatr.getdata <- function(group, feed)
{
	# get all the members of the group
	names <- tolower(splatr.gselect(group, "member"))
	# get data for each name
	for (i in seq(along = names))
	{
		# send request
		dframe <- do.call(feed$getfn, list(names[i], feed))
		# process response
		do.call(feed$gotfn, list(names[i], feed, dframe))
	}
}