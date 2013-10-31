##############################################################
#
# Package   : splatr
# Module    : feed.yahoo
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getdata.yahoo <-
	function(name, feed)
{
   daysinmonth <- 30
	startdate <- feed$startdate
	startmonth <- as.integer(months(startdate - daysinmonth))
	startday <- days(startdate)
	startyear <- years(startdate)
	enddate <- feed$enddate
	endmonth <- as.integer(months(enddate - daysinmonth))
	endday <- days(enddate)
	endyear <- years(enddate)
	dstr <- paste("&a=", startmonth,
                 "&b=", startday,
                 "&c=", startyear,
                 "&d=", endmonth,
                 "&e=", endday,
                 "&f=", endyear,
                 "&g=d&ignore=.csv", sep="")
	request <- paste(feed$uri, "/table.csv?s=", name, dstr, sep="")
	as.data.table(read.csv(request))
}

splatr.gotdata.yahoo <-
	function(name, feed, dataframe)
{
   snames <- names(splatr.getglobalstates(feed$name, feed$subject))
   setnames(dataframe, 1:length(snames), snames)
   setkey(dataframe, "date")
   dataframe$date <- as.character(with(dataframe, splatr.chrondate(date)))
   fname <- splatr.getframename(name, feed$subject, feed$space)
   splatr.setframe(fname, dataframe)
}
