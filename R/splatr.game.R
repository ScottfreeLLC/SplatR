##############################################################
#
# Package   : splatr
# Module    : game
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

setMethod("initialize", "splatr.game",
	function(.Object,
			   name,
			   subject,
			   group,
			   gdir,
			   ...)
{
   gname <- splatr.getgroupname(name, subject)
	group <- splatr.getgroup(gname)
	if (!exists(group))
		stop("Could not find group:", name, "\n")
	else {
      dname <- splatr.getdirectoryname(name, group$mclass)
		gdir <- splatr.getdirectory(dname)
		if (!exists(gdir))
			stop("Could not find directory for:", dname, "\n")
	}
	callNextMethod(.Object,
					   name = name,
					   subject = subject,
					   group = group,
					   gdir = gdir,
					   ...)
})
	
splatr.newgame <- function(name, subject)
{
	game <- new("splatr.game",
				   name = name,
				   subject = subject)
	game
}

splatr.rungame <-
	function(game,
			   startdate,
			   enddate = date(),
			   keys = c("home", "away"),
			   matchup = c(1, 2),
			   fractal = "g1")
{
	games <- game$gdir[date >= startdate & date <= enddate, ]
	ngames <- nrow(games)
	gstates <- games
	gnames <- names(splatr.getstates(game$subject))
	gspace <- splatr.newspace("game", "states", fractal)
   gname <- splatr.getframename(game$name, game$subject, gspace)
	splatr.setframe(gname, gstates)
	teams <- splatr.gselect(game$group, "member")
	for (i in seq(along = keys))
	{
		svec <- get(paste("gstates", keys[i], sep="$"))
		mvec <- match(svec, teams)
		kframe[i] <- as.data.table(matrix(nrow = ngames))
		for (j in seq(along = teams))
		{
			tframe <- splatr.getframe(paste(teams[j], game$name, sep="."), gframe)
			if (!exists(tframe))
				stop("Could not find game data for:", teams[j], "\n")
			else {
				tframe <- tframe[date >= startdate & date <= enddate, ]
				kframe[i][which(mvec == j), ] <- tframe[1:nrow(tframe), ]
			}
		}
		names(kframe[i]) <- paste(keys[i], gnames, sep="$")
		gstates <- data.table(gstates, kframe)
	}
	mframe <- kframe[matchup[1]] - kframe[matchup[2]]
	names(mframe) <- paste(gnames, "delta", sep=".")
	gstates <- data.table(gstates, mframe)
	gstates
}

splatr.gamelab <-
	function(group,
			   output = "margin",
			   inputs = ".",
			   method = splatr.mrmethod,
			   lag = 1,
			   groupby = c("year"),
			   gspace = splatr.newspace("game", "states", "g1"))
{
	splatr.predict(group,
					   output,
					   inputs,
					   method,
					   lag,
					   groupby,
					   gspace)
   rm(gspace)
}
