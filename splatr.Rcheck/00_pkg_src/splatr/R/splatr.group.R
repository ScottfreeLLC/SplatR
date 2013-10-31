setClass("splatr.group",
	contains = "splatr",
	representation(
		dynamic = "logical",
		recursive = "logical",
		members = "character",
		gclass = "character",
		mclass = "character",
		gframe = "splatr.frame",
		gfn = "function",
		gfnopt = "character",
		gfnagg = "logical"),
	validity = splatr.validgroup
)

splatr.newgroup <-
	function(name = "mygroup", 
			  subject = "stock",
			  dynamic = TRUE,
			  recursive = FALSE,
			  members = c(),
			  gclass = "splatr.group",
			  mclass = "character",
			  gframe = splatr.newframe("price", "states", "d1"),
			  gfn = splatr.gapply,
			  gfnopt = "",
			  gfnagg = FALSE,
			  genstates = FALSE)
{
	gobject <- splatr.getgroup(name, subject)
	if (is.null(gobject))
	{
		# create group object
		value <- new("splatr.group",
					name = name,
					subject = subject,
					dynamic = dynamic,
					recursive = recursive,
					members = members,
					gclass = gclass,
					mclass = mclass,
					gframe = gframe,
					gfn = gfn,
					gfnopt = gfnopt,
					gfnagg = gfnagg)
		if (!is.null(value)) {
			# set group name
			splatr.setgroup(name, subject, value)
			# load members from fixed groups
			if (!dynamic) {
				dir <- splatr.getdirectory(name, mclass)
				members <- with(dir, dir@key)
			}
			# set up states objects
			if (genstates) {
				sframe <- gframe
				sframe@fclass <- "states"
				sframe@fdata <- gclass
				gstates <- splatr.setgroupstates(name, subject, gclass, sframe)
				sframe@fdata <- mclass
				mstates <- splatr.setgroupstates(name, subject, mclass, sframe)
			}
		}
		value
	}
	else {
		cat("Group", splatr.getgroupname(name, subject), "of class", class(gobject), "already defined\n")
	}
}

splatr.getgroupnames <- function(members)
{
	sapply(1:length(members), function(i) splatr.getname(members[i]))
}

splatr.getgroupname <- function(name, subject)
{
	gname <- paste(name, subject, sep=".")
	gname
}

splatr.setgroup <- function(name, subject, gobject)
{
	gname <- splatr.getgroupname(name, subject)
	assign(gname, gobject, envir = .GlobalEnv)
	gname
}

splatr.getgroup <- function(name, subject)
{
	gname <- splatr.getgroupname(name, subject)
	splatr.getsmart(gname)
}

splatr.setgroupframe <- function(g, frame)
{
	g@gframe <- frame
}

splatr.addgroup <- function(g, mnew)
{
	if (g@dynamic) {
		mold <- g@members
		n <- length(mold)
		gids <- grep("\\.", mold)
		if (!g@recursive & length(gids) > 0)
			cat("Cannot add group members to non-recursive groups\n")
		else {
			inset <- mnew %in% mold
			if (all(inset))
				cat("All members already in set\n")
			else {
				gname <- splatr.getgroupname(g@name, g@subject)
				mname <- paste(gname, "@members", sep="")
				expr <- paste(mname, " <<- sort(union(mold, mnew))", sep="")
				eval(parse(text = expr))
			}
		}
	}
	else
		cat("Cannot add members to a non-dynamic group\n")
}

splatr.removegroup <- function(g, mexist)
{
	if (g@dynamic) {
		mold <- g@members
		inset <- mexist %in% mold
		if (any(inset)) {
			gname <- splatr.getgroupname(g@name, g@subject)
			mname <- paste(gname, "@members", sep="")
			expr <- paste(mname, " <<- setdiff(mold, mexist)", sep="")
			eval(parse(text = expr))
		}
		else
			cat("No members found to remove\n")
	}
	else
		cat("Cannot remove members from a non-dynamic group\n")
}

splatr.lookupgroup <- function(g, name)
{
	g@members[which(name %in% g@members)]
}

splatr.gselect <- function(g, type = "all")
{
	results <- c()
	
	gselect.tree <- function(g)
	{
		gn <- g@name
		gs <- g@subject
		members <- g@members
		n <- length(members)
		nseq <- seq(1:n)
		gids <- grep("\\.", members)
		ngids <- length(gids)
		if (ngids > 0)
			mids <- nseq[-gids]
		else
			mids <- nseq
		if (ngids > 0) {
			for (i in seq(along = gids))
			{
				gm <- members[gids[i]]
				gn <- splatr.nparse(gm, "oid")
				gs <- splatr.nparse(gm, "oclass")
				gg <- splatr.getgroup(gn, gs)
				if (type != "group")
					results <- c(results, members[mids], gselect.tree(gg))
				if (type != "member")
					results <- c(results, members[gids], gselect.tree(gg))
			}
		}
		else {
			if (type != "group")
				results <- c(results, members[mids])
			if (type != "member")
				results <= c(results, members[gids])
		}
		return(results)
	}
	
	results <- gselect.tree(g)
	unique(results)
}

splatr.gapply <- function(group, gobject, ...)
{
	gfn <- group@gfn
	gnames <- splatr.gselect(group, "member")
	for (i in seq(along = gnames))
		aobject <- do.call(gfn, list(gobject, gnames[i], ...))
}