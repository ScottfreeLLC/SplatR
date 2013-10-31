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
			  dir = splatr.getdirectoryname(name, subject),
			  startdate = splatr.getdate(365),
			  enddate = splatr.getdate(),
	        fframe = splatr.newframe("price", "states", "d1"),
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
				dir = dir,
				startdate = startdate,
				enddate = enddate,
				fframe = fframe,
				getfn = getfn,
				gotfn = gotfn)
   if (!is.null(nf)) {
      fframe@fclass = "feed"
      splatr.setframename(name, subject, fframe, nf)
   }
   rm(fframe)
}

splatr.getdata <-
	function(group, feed)
{
	# get all the members of the group
	names <- tolower(splatr.gselect(group, "member"))
	# get data for each name
	for (i in seq(along = names))
	{
		# send request
		dframe <- do.call(feed@getfn, list(names[i], feed))
		# process response
		dframe <<- do.call(feed@gotfn, list(names[i], feed, dframe))
	}
}