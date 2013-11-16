##############################################################
#
# Package   : splatr
# Module    : frame
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.setframe <- function(fname, fobject)
{
   assign(fname, fobject, envir = .GlobalEnv)
   fname
}

splatr.getframe <- function(fname)
{
   splatr.getref(fname)
}

splatr.getframename <- function(name, subject, space)
{
	fname <- paste(name,
					   subject,
					   space$what,
					   space$wherein,
					   space$fractal,
					   sep=".")
	fname
}

splatr.mergeframes <- function(group, fname)
{
   gnames <- splatr.gselect(group, "member")
   gspace <- group$space
   mframe <- data.table()
   for (i in seq(along=gnames))
   {
      fn <- splatr.getframename(gnames[i], group$subject, gspace)
      f <- splatr.getframe(fn)
      mframe <- rbind(mframe, f)
   }
   mframe <- mframe[order(date), ]
   assign(fname, mframe, envir = .GlobalEnv)
}

splatr.splitframe <- function(group)
{
	if (is(group, "splatr.group"))
	{
		gnames <- splatr.gselect(group, "member")
		gspace <- group$space
      dname <- splatr.getframename(group$name, group$subject, gspace)
		dframe <- splatr.getframe(dname)
		for (i in seq(along=gnames))
		{
			newframe <- dframe[name == gnames[i],]
         fname <- splatr.getframename(gnames[i], group$subject, gspace)
			splatr.setframe(fname, newframe)
		}
	}
	else
		cat("Frame to split must be a group\n")
}