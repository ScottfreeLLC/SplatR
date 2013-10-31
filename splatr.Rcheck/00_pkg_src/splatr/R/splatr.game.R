setMethod("initialize", "splatr.game",
	function(.Object,
			  name,
			  subject,
			  group,
			  gdir,
			  ...)
{
	group <- splatr.getgroup(name, subject)
	if (!exists(group))
		stop("Could not find group:", name, "\n")
	else {
		gdir <- splatr.getdirectory(name, group@mclass)
		if (!exists(gdir))
			stop("Could not find directory for:",
				  splatr.getdirectoryname(name, group@mclass), "\n")
	}
	callNextMethod(.Object,
					 name = name,
					 subject = subject,
					 group = group,
					 gdir = gdir,
					 ...)
})
	
splatr.newgame <-
	function(name,
			  subject)
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
	games <- game@gdir[date >= startdate & date <= enddate, ]
	ngames <- nrow(games)
	gstates <- games
	gnames <- names(splatr.getstates(game@subject))
	gframe <- splatr.getframe("game", "states", fractal)
	splatr.setframename(game@name, gframe, gstates)
	teams <- splatr.gselect(game@group, member)
	for (i in seq(along = keys))
	{
		svec <- get(paste("gstates", keys[i], sep="$"))
		mvec <- match(svec, teams)
		kframe[i] <- data.frame(nrow = ngames)
		for (j in seq(along = teams))
		{
			tframe <- splatr.getframe(paste(teams[j], game@name, sep="."), gframe)
			if (!exists(tframe))
				stop("Could not find game data for:", teams[j], "\n")
			else {
				tframe <- tframe[date >= startdate & date <= enddate, ]
				kframe[i][which(mvec == j), ] <- tframe[1:nrow(tframe), ]
			}
		}
		names(kframe[i]) <- paste(keys[i], gnames, sep="@")
		gstates <- data.frame(gstates, kframe)
	}
	mframe <- kframe[matchup[1]] - kframe[matchup[2]]
	names(mframe) <- paste(gnames, "delta", sep=".")
	gstates <- data.frame(gstates, mframe)
	gstates
}

splatr.gamelab <-
	function(group,
			  responses = "margin",
			  variables = ".",
			  leadvars = "",
			  method = new("splatr.mrmethod"),
			  lag = 1,
			  groupby = c("year"),
			  dframe = splatr.newframe("game", "states", "g1"))
{
	splatr.predict(group,
					   responses,
					   variables,
					   leadvars,
					   method,
					   lag,
					   groupby,
					   dframe)
   rm(dframe)
}
