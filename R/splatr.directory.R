splatr.newdirectory <-
	function(name = "nasdaq",
			   subject = "stock",
			   uri = "ftp://ftp.nasdaqtrader.com/symboldirectory/nasdaqlisted.txt",
			   sep = "|",
			   key = "Symbol")
{
	dname <- ""
	newdir <- new("splatr.directory",
				    name = name,
				    subject = subject,
				    uri = uri,
				    sep = sep,
				    key = key)
	if (!is.null(newdir)) {
		newdir$frame <- read.csv(uri, sep=sep)
      dname <- splatr.getdirectoryname(name, subject)
		splatr.setdirectory(dname, newdir)
	}
	dname
}

splatr.getdirectoryname <- function(name, subject)
{
	dname <- paste(name, ".", subject, "s", sep="")
	dname
}

splatr.setdirectory <- function(dname, dobject)
{
	assign(dname, dobject, envir = .GlobalEnv)
	dname
}

splatr.getdirectory <- function(dname)
{
	try(get(dname), silent=TRUE)
}

splatr.dlookup <- function(dir, keyvalue)
{
	dirf <- dir$frame
	with(dirf, dirf[get(dir$key) == keyvalue, ])
}