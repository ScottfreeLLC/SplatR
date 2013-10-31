splatr.getname <- function(object)
{
	ifelse(is(object, "character"), object, object$name)
}

splatr.nparse <- function(name, component="oid")
{
	subnames <- strsplit(name, "\\.")
	clist <- subnames[[1]]
	nc <- length(clist)
	switch(component,
			oid = clist[1],
			oclass = clist[2],
			oname = paste(clist[1], clist[2], sep="."),
			fractal = clist[nc-2],
			what = clist[nc-1],
			wherein = clist[nc],
			fname = paste(clist[nc-2], clist[nc-1], clist[nc], sep="."),
			date = clist[nc],
		stop("Invalid component ", component, "\n")
	)
}