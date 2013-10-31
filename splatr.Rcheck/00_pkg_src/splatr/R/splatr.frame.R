setClass("splatr.frame",
	representation(
		fdata = "character",
		fclass = "character",
		fractal = "character"),
	validity = splatr.validframe)
	
splatr.newframe <-
	function(fdata = "price",
			  fclass = "states",
			  fractal = "d1")
{
	f <- new("splatr.frame",
			  fdata = fdata,
			  fclass = fclass,
			  fractal = fractal)
	f
}

splatr.getframename <- function(name, subject, frame)
{
	fname <- paste(name,
					 subject,
					 frame@fdata,
					 frame@fclass,
					 frame@fractal,
					 sep=".")
	fname
}

splatr.setframename <- function(name, subject, frame, fobject)
{
	fname <- splatr.getframename(name, subject, frame)
	assign(fname, fobject, envir = .GlobalEnv)
	fname
}

splatr.getframe <- function(name, subject, frame)
{
	fname <- splatr.getframename(name, subject, frame)
	splatr.getsmart(fname)
}

splatr.mergeframes <- function(name, subject, frame, fids)
{
	tempframe <- frame
	newframe <- data.frame()
	splatr.setframename(name, subject, frame, newframe)
	for (i in seq(along=fids))
	{
		tempframe@fdata <- fids[i]
		merge(newframe, splatr.getframe(name, subject, tempframe))
	}
	newframe <- newframe[order(date, name),]
	newframe
}

splatr.splitframe <- function(group)
{
	if (is(group, "splatr.group"))
	{
		gnames <- splatr.getgroupnames(splatr.gselect(group, "member"))
		gframe <- group@gframe
		df <- splatr.getframe(group@name, group@subject, gframe)
		for (i in seq(along=gnames))
		{
			nf <- df[name == gnames[i],]
			splatr.setframename(gnames[i], group@subject, gframe, nf)
		}
	}
	else
		cat("Frame to split must be a group\n")
}