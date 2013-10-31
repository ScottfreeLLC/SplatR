##############################################################
#
# Package   : splatr
# Module    : group
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

setRefClass("splatr.group",
	contains = "splatr",
	fields = list(
		dynamic = "logical",
		recursive = "logical",
		members = "character",
		gstate = "character",
		mstate = "character",
		space = "splatr.space",
		gfn = "function",
		gfnagg = "logical"))

splatr.newgroup <-
	function(name = "mygroup", 
			   subject = "stock",
			   dynamic = TRUE,
			   recursive = FALSE,
			   members = as.character(c()),
            objects = list(),
			   gstate = "text",
			   mstate = "text",
			   space = splatr.newspace("price", "states", "d1"),
			   gfn = splatr.gapply,
			   gfnagg = FALSE,
			   genstates = FALSE)
{
   gname <- splatr.getgroupname(name, subject)
	gobject <- splatr.getgroup(gname)
	if (is.null(gobject))
	{
		# create group object
		gnew <- new("splatr.group",
					   name = name,
					   subject = subject,
					   dynamic = dynamic,
					   recursive = recursive,
					   members = members,
					   gstate = gstate,
					   mstate = mstate,
					   space = space,
					   gfn = gfn,
					   gfnagg = gfnagg)
		if (!is.null(gnew)) {
			# set group name
			splatr.setgroup(gname, gnew)
			# load members from fixed groups
			if (!dynamic) {
            dname <- splatr.getdirectoryname(name, subject)
				dir <- splatr.getdirectory(dname)
				members <- with(dir, dir$key)
			}
			# set up states objects
			if (genstates) {
            gspace <- splatr.newspace(gstate, "states", space$fractal)
				gstates <- splatr.setgroupstates(gnew$name, gnew$subject, gspace)
            mspace <- splatr.newspace(mstate, "states", space$fractal)
				mstates <- splatr.setgroupstates(gnew$name, gnew$subject, mspace)
			}
		}
	}
	else {
		cat("Group", gname, "of class", class(gobject), "already defined\n")
	}
   gname
}

splatr.getgroupname <- function(name, subject)
{
	gname <- paste(name, subject, sep=".")
	gname
}

splatr.setgroup <- function(gname, gobject)
{
	assign(gname, gobject, envir = .GlobalEnv)
	gname
}

splatr.getgroup <- function(gname)
{
	splatr.getref(gname)
}

splatr.addgroup <- function(g, mnew)
{
   
	if (g$dynamic) {
		mold <- g$members
		n <- length(mold)
      agroup <- sapply(seq(along = mnew),
                       function(i) is(splatr.getref(mnew[i]), "splatr.group"))
      if (any(agroup) & !g$recursive)
         cat("Cannot add group members to non-recursive groups\n")
      else {
         inset <- FALSE
         if (n > 0) {
            inset <- mnew %in% mold
            mnew <- mnew[!inset]
         }
         if (all(inset))
            cat("All members already in set\n")
         else {
            g$members <- unique(sort(union(mold, mnew)))
            cat("Added: ", mnew, "\n")            
         }
      }
	}
	else
		cat("Cannot add members to a non-dynamic group\n")
}

splatr.removegroup <- function(g, mexist, remove = FALSE)
{
   if (g$dynamic) {
      mold <- g$members
		inset <- mexist %in% mold
		if (any(inset)) {
		   mexist <- mexist[inset]
		   g$members <- setdiff(mold, mexist)
         if (remove)
            do.call(rm, list(mexist), envir = .GlobalEnv)           
		   cat("Removed: ", mexist, "\n")
		}
		else
			cat("No members found to remove\n")
	}
	else
		cat("Cannot remove members from a non-dynamic group\n")
}

splatr.glookup <- function(g, name)
{
	g$members[match(name, g$members)]
}

splatr.gselect <- function(g, type = "all")
{
	results <- c()
	
	gselect.tree <- function(g)
	{
		gn <- g$name
		gs <- g$subject
		members <- g$members
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
				gg <- splatr.getgroup(splatr.getgroupname(gn, gs))
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

splatr.gapply <- function(group, object, ...)
{
	gnames <- splatr.gselect(group, "member")
	for (i in seq(along = gnames))
		results <- do.call(group$gfn, list(object, gnames[i], ...))
   results
}