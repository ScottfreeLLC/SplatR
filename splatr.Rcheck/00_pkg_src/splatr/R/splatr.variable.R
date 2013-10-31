splatr.getvariable <- function(vname)
{
	svar <- splatr.variables[[vname]]
	var <- ifelse(is.null(svar), vname, svar)
	var
}

splatr.deletevariable <- function(vname)
{
	eval(parse(text=paste("splatr.variables$", vname, " <<- NULL", sep="")))
}

splatr.newvariable <-
	function(vname,
			  vexpr,
			  replace=FALSE)
{
	success <- FALSE
	vfound <- splatr.variables[[vname]]
	vexist <- ifelse(is.null(vfound), FALSE, TRUE)
	if (vexist & replace) {
		splatr.deletevariable(vname)
		vexist <- FALSE
	}
	if (vexist)
		cat("Variable", vname, "already exists\n")
	else {
		index <- match(vexpr, splatr.variables)
		if (!is.na(index)) {
			cat("Variable", vname, "has an identical expression as variable",
				names(splatr.variables[index]), "\n")
		}
		else {
			splatr.variables[[vname]] <<- vexpr
			success <- TRUE
		}
	}
	if (success)
	list(vname, vexpr)
}

splatr.vparse <- function(vname)
{
	# split the variable name into its components
	varsplit <- strsplit(vname, "\\.")
	vlen <- nchar(vname)
	vlist <- varsplit[[1]]
	vparts <- length(vlist)
	# extract vroot and possibly period
   v1 <- vlist[1]
   period <- 0
   pos <- regexpr(".[0-9]+$", v1)
   if (pos > 0) {
      start <- pos[1]
      end <- nchar(v1)
      period <- as.integer(substring(v1, first=start+1, last=end))      
   }
   vroot <- ifelse(period > 0, substring(v1, 1, start), v1)      
	# extract lag
	lag <- 0
	if (vparts > 1) {
		pos <- regexpr("^[0-9]+$", vlist[2])
		lag <- ifelse(pos[1] > 0, as.integer(vlist[2]), 0)
	}
	# extract rperiod
	pos <- regexpr("\\.run[0-9]+", vname)
	start <- pos[1]
	rperiod <- substring(vname, first=start+4, last=start+attr(pos, "match.length")-1)
	rperiod <- ifelse(start > 0, as.integer(rperiod), 0)
	# extract fractal
	fractal <- vlist[vparts]
	fractal <- ifelse(splatr.isfractal(fractal), fractal, "")
	# return components
	list(vroot = vroot,
		  period = period,
		  lag = lag,
        vxlag = v1,
		  fractal = fractal,
		  rperiod = rperiod)
}

splatr.vnames <- function(vname)
{
	allvars <- c()
	
	vnames.tree <- function(vname)
	{
      vtext <- splatr.getvariable(vname) 
		allvars <- all.vars(parse(text=vtext))
		vnames <- unique(allvars)
		for (i in seq(along = vnames))
		{
         vlist <- splatr.vparse(vnames[i])
         vroot <- vlist[["vroot"]]
         vxlag <- vlist[["vxlag"]]
         period <- vlist[["period"]]
         if (period > 0)
            allvars <- c(vxlag, allvars)
			if (vroot %in% names(splatr.variables))
				allvars <- c(vnames.tree(vroot), vroot, allvars)
			else
				allvars <- c(vnames[i], allvars)
		}
		return(allvars)
	}

	allvars <- c(vnames.tree(vname), vname)
	unique(allvars)
}

splatr.vroll <- function(var, period, by = NULL, rollfn = NULL, ...)
{
   rollfn <- match.fun(rollfn)
   if (is.null(by)) by <- period
   length <- length(var)
   seq1 <- seq(1, length - period + 1, by = by)
   seq2 <- lapply(seq1, function(i) i:(i + period - 1))
   vout <- lapply(seq2, function(i) rollfn(var[i], ...))
   c(rep(NA, period - 1), unlist(vout))
}

splatr.vstep <- function(var, stepfn = NULL, ...)
{
   stepfn <- match.fun(stepfn)
   length <- length(var)
   seq1 <- seq(1, length)
   seq2 <- lapply(seq1, function(i) 1:i)
   vout <- lapply(seq2, function(i) stepfn(var[i], ...))
   unlist(vout)
}

splatr.vapply <- function(g, gframe, vars)
{
	gnames <- tolower(splatr.gselect(g, "member"))
	glen <- length(gnames)
	for (i in seq(along=vars))
	{
		var <- vars[i]
		cat("Applying variable", var, "to", glen, "frames:", gnames, "\n")
		vnames <- splatr.vnames(var)
		for (j in seq(along=gnames))
		{
			f <- splatr.getframe(gnames[j], g@subject, gframe)
			fnames <- names(f)
			addnames <- vnames[!vnames %in% fnames]
         for (k in seq(along = addnames))
         {
            fname <- splatr.getframename(gnames[j], g@subject, gframe)
            vname <- addnames[k]
            vlist <- splatr.vparse(vname)
            vxlag <- vlist[["vxlag"]]
            # extract the variable root
            vroot <- vlist[["vroot"]]
            vexpr <- splatr.getvariable(vroot)
            allnames <- all.names(parse(text = vexpr))
            # extract the variable period, if any
            period <- vlist[["period"]]
            pstring <- "period"
            if (!is.na(match(pstring, allnames))) {
               if (period == 0) period <- 1
               vexpr <- gsub(pstring, period, vexpr)
            }
            # determine whether the function is an R vector function
            allvars <- all.vars(parse(text = vexpr))
            func <- setdiff(allnames, allvars)
            fnopts <- ", na.rm = TRUE"
            pos <- match(func, splatr.vectorfunctions)
            # extract the lag
            lag <- vlist[["lag"]]
            expr1 <- paste(fname, " <<- transform(", fname, ", ", sep = "")
            # now choose one of four alternatives: lag, roll, step, base
            if (lag > 0)
            {
               expr <- paste(expr1, vname, " = splatr.shift(", vxlag, ", ", -lag, "))", sep="")
               cat("Lag Variable: ", vname, ", Expression: ", expr, "\n")
            }
            else if (period > 0)
            {
               # rolling variable
               expr <- paste(expr1, vxlag, " = splatr.vroll(", allvars[1], ", ", period, ", 1, ", func, fnopts, "))", sep="")
               cat("Rolling Variable: ", vxlag, ", Expression: ", expr, "\n")
            }
            else if (!is.na(pos[1]))
            {               
               # stepping variable
               expr <- paste(expr1, vxlag, " = splatr.vstep(", allvars[1], ", ", func, fnopts, "))", sep="")
               cat("Step Variable: ", vxlag, ", Expression: ", expr, "\n")
            }
            else
            {
               # base variable
               expr <- paste(expr1, vroot, " = ", vexpr, ")", sep="")
               cat("Base Variable: ", vname, ", Expression: ", expr, "\n")
            }
            eval(parse(text = expr))
         } # end add loop
		} # end names loop
	} # end variables loop
}

splatr.vunapply <- function(g, gframe, vars)
{
   gnames <- tolower(splatr.gselect(g, "member"))
   glen <- length(gnames)
   for (i in seq(along=vars))
   {
      var <- vars[i]
      cat("Deleting variable", var, "from", glen, "frames:", gnames, "\n")
      for (j in seq(along=gnames))
      {
         fname <- splatr.getframename(gnames[j], g@subject, gframe)
         expr <- paste(fname, "$", var, "<<- NULL", sep="")
         eval(parse(text = expr))
     }
   }
}

# > vlapply(n, f, "cma50.1", ">", 3)   # cma50.1.gt3 "cma50.1 > cma50.4"

splatr.vlapply <- function(g, gframe, vars, vop, vlag)
{
	rops <- c(">", ">=", "<", "<=", "==", "!=")
	tops <- c("gt", "ge", "lt", "le", "eq", "ne")
	pos <- which(vop %in% rops)
	optext <- ifelse(pos > 0, tops[pos[1]], tops[1])
	newvars <- c()
	for (i in seq(along = vars))
	{
		vl <- vars[i]
		vlist <- splatr.vparse(vl)
		newlag <- vlist[["lag"]] + vlag
		vr <- paste(vlist[["vxlag"]], newlag, sep=".")
		vname <- paste(vl, ".", optext, vlag, sep="")
		vtext <- paste(vl, rops[pos[1]], vr, sep=" ")
		splatr.newvariable(vname, vtext)
		newvars <- c(newvars, vname)
	}
	splatr.vapply(g, gframe, newvars)
	newvars
}

# > vrapply(n, f, "cma1.50.gt3", 20)   # cma1.50.gt3.run20 "runs(cma1.50.gt3)"

splatr.vrapply <- function(aname, aframe, vars, runlength)
{
	newvars <- c()
	for (i in seq(along=vars))
	{
		vname <- paste(vars[i], ".run", runlength, sep="")
		vtext <- paste("runs (", vars[i], ")", sep="")
		splatr.newvariable(vname, vtext)
		newsvars <- c(newvars, vname)
	}
	splatr.vapply(aname, aframe, newvars)
	newvars
}

