##############################################################
#
# Package   : splatr
# Module    : fractal
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

# Global Variables

splatr.trading.factors <<- c(1000, 60, 60, 24, 7, 4, 3, 4, NA)
splatr.trading.levels <<- c("tick", "second", "minute", "hour", "day", "Week", "Month", "Quarter", "Year")
splatr.trading.abbrevs <<- substr(splatr.trading.levels, 1, 1)
splatr.trading.fractals <<- ordered(splatr.trading.levels, levels=splatr.trading.levels)

# Functions

splatr.fparse <- function(fractal)
{
	fregexpr <- "^(Y|Q|M|w|d|h|m|s|t){1}[0-9]+$"
	pos <- regexpr(fregexpr, fractal)
	if (pos > 0) {
		f <- splatr.trading.fractals[which(splatr.trading.abbrevs == substr(fractal, 1, 1))]
		fn <- as.integer(substr(fractal, 2, nchar(fractal)))
	}
	else {
		f <- NULL
		fn <- 0
	}
	list(f=f, n=fn)
}

splatr.isfractal <- function(fractal)
{
	f <- splatr.fparse(fractal)
	ifelse(!is.null(f[["f"]]), TRUE, FALSE)
}

# > fracfac("d1", "m10")		# d1/m10 = 1 x 24 x 60 / 10 = 144
# > fracfac("h5", "s3000")	   # h5/s3000 = 5 x 60 x 60 / 3000 = 6

splatr.fracfac <- function(fexprn, fexprd)
{
	posn <- match(fexprn[["f"]], splatr.trading.fractals)
	posd <- match(fexprd[["f"]], splatr.trading.fractals)
	if (posn >= posd) {
		num <- fexprn[["n"]] * prod(splatr.trading.factors[posd:(posn-1)])
		den <- fexprd[["n"]]
		ffac <- num %/% den
	}
	else
		ffac <- 0
	return(ffac)
}

splatr.genfrac <- function(date, fracexpr)
{
	fpart <- fracexpr[["f"]]
	fnum <- fracexpr[["n"]]
	switch(fpart,
			Y = date$year,
			Q = as.integer(paste(date$year, quarter(date), sep="")), # calendar package
			M = as.integer(paste(date$year, date$mon, sep="")),
			W = as.integer(paste(date$year, week(date), sep="")),
			d = rd(date),
			h = as.integer(paste(rd(date), date$hour, sep="")),
			m = as.integer(paste(rd(date), date$hour, date$min, sep="")),
			s = date,
			t = date,
		stop("Invalid fractal"))		#end switch
}

splatr.mergefractals <-
	function(name,
            subject,
            aspace = splatr.newspace("price", "states", "d1"),
            cfractal = "m5",
            cmultiple = 2)
{
	aframe <- splatr.getframe(name, subject, aspace)
	cspace <- pspace
	cspace$fractal <- cfractal
	cframe <- splatr.getframe(name, subject, cspace)
	afrac <- splatr.fparse(aframe$fractal)	# afrac[["f"]], afrac[["n"]]
	cfrac <- splatr.fparse(cfractal)		   # cfrac[["f"]], cfrac[["n"]]
	ffac <- splatr.fracfac(afrac, cfrac)
	if (ffac < 2) stop("Fractal multiple must be >= 2")
	if (cmultiple > ffac) stop("Cutoff multiple is too high")
	avec <- with(af, splatr.genfrac(date, afrac))
	cvec <- (with(cf, splatr.genfrac(date, cfrac)) %/% cfrac[["n"]]) + 1
	clengths <- rle(cvec)[[1]]
	cvalues <- rle(cvec)[[2]]
	nruns <- length(cvalues)
	coffsets <- cumsum(clengths) - clengths + cmultiple
	af <- cbind(af, cf[coffsets])
	af
}