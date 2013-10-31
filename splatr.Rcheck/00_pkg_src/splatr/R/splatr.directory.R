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
		newdir@frame <- read.csv(uri, sep=sep)
		dname <- splatr.setdirectory(name, subject, newdir)
	}
	dname
}

splatr.getdirectoryname <- function(name, subject)
{
	dname <- paste(name, subject, sep=".")
	dname <- paste(dname, "s", sep="")
	dname
}

splatr.setdirectory <- function(name, subject, dobject)
{
	dname <- splatr.getdirectoryname(name, subject)
	assign(dname, dobject, envir = .GlobalEnv)
	dname
}

splatr.getdirectory <- function(name, subject)
{
	dname <- splatr.getdirectoryname(name, subject)
	try(get(dname), silent=TRUE)
}

splatr.dlookup <- function(dir, keyvalue)
{
	dirf <- dir@frame
	with(dirf, dirf[get(dir@key) == keyvalue, ])
}