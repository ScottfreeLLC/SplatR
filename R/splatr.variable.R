##############################################################
#
# Package   : splatr
# Module    : variable
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

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
            replace = FALSE,
            allowsame = FALSE)
{
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
		if (!is.na(index) & !allowsame)
			cat("Variable", vname, "has an identical expression as variable",
             names(splatr.variables[index]), "\n")
		else
			splatr.variables[[vname]] <<- vexpr
	}
	vname
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
   # extract the function
	vexpr <- splatr.getvariable(vroot)
	allnames <- all.names(parse(text = vexpr))
	allvars <- all.vars(parse(text = vexpr))
	func <- setdiff(allnames, allvars)
	# return components
	list(vroot = vroot,
		  period = period,
		  lag = lag,
        vxlag = v1,
		  fractal = fractal,
		  rperiod = rperiod,
        func = func,
        fps = allvars)
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

splatr.vrun <- function(var, runfn = NULL, ...)
{
   runfn <- match.fun(runfn)
   length <- length(var)
   seq1 <- seq(1, length)
   seq2 <- lapply(seq1, function(i) 1:i)
   vout <- lapply(seq2, function(i) runfn(var[i], ...))
   unlist(vout)
}

splatr.vapply <- function(g, gspace, vars)
{
   # get the group members
	gnames <- tolower(splatr.gselect(g, "member"))
	glen <- length(gnames)
	# primitive expression strings
	ostring <- "open"
	hstring <- "high"
	lstring <- "low"
	cstring <- "close"
	pstring <- "period"
	hhstring <- "highesthigh"
	llstring <- "lowestlow"
	# apply each of the variables
	for (i in seq(along=vars))
	{
		var <- vars[i]
		cat("Applying variable", var, "to", glen, "frames:", gnames, "\n")
		vnames <- splatr.vnames(var)
		for (j in seq(along=gnames))
		{
         # get frame information
         fname <- splatr.getframename(gnames[j], g$subject, gspace)
			f <- splatr.getframe(fname)
			fnames <- names(f)
         # add "highesthigh" and "lowestlow" to anticipate range variables
         vnames <- c(hhstring, llstring, vnames)
         addnames <- vnames[!vnames %in% fnames]
         # apply each variable to the given frame
         for (k in seq(along = addnames))
         {
            vname <- addnames[k]
            # parse the variable name
            vlist <- splatr.vparse(vname)
            vxlag <- vlist[["vxlag"]]
            # extract the variable root
            vroot <- vlist[["vroot"]]
            vexpr <- splatr.getvariable(vroot)
            allnames <- all.names(parse(text = vexpr))
            allvars <- all.vars(parse(text = vexpr))
            # extract the variable period, if any
            period <- vlist[["period"]]
            # extract the lag
            lag <- vlist[["lag"]]
            # extract the function and its parameters
            func <- vlist[["func"]]
            fps <- vlist[["fps"]]
            # determine the name and type of function
            posv <- match(func, splatr.vectorfunctions)
            # define transformation expression components
            expr1 <- paste(fname, " <<- transform(", fname, ", ", sep = "")
            fnopts <- ", na.rm=TRUE"
            # check for variable existence
            vexist <- vname %in% colnames(f)
            # check for no more variable reduction
            vequal <- identical(vroot, vexpr)
            #
            # Variables are one of the following types:
            #
            #    Lag (period or no period)
            #    Rolling (period)
            #    Running (period)
            #    Relative (period)
            #    Range (period)
            #    Base (no period)
            #
            if (!vexist & !vequal)
            {               
               if (!is.na(posv[1]))
               {
                  # R vector function found, so variable is either rolling or running
                  if (period > 0)
                  {
                     # Rolling Variable
                     expr <- paste(expr1, vxlag, " = splatr.vroll(", fps[1], ", ", period, ", 1, ", func, fnopts, "))", sep="")
                     cat("Rolling Variable: ", vxlag, ", Expression: ", expr, "\n") 
                  }
                  else
                  {
                     # Running Variable
                     expr <- paste(expr1, vxlag, " = splatr.vrun(", fps[1], ", ", func, fnopts, "))", sep="")
                     cat("Running Variable: ", vxlag, ", Expression: ", expr, "\n")
                  }
               }
               else
               {
                  if (pstring %in% allvars)
                  {
                     # Relative Variable
                     vexpr <- gsub(pstring, period, vexpr)
                     expr <- paste(expr1, vxlag, " = ", vexpr, ")", sep="")
                     cat("Relative Variable: ", vxlag, ", Expression: ", expr, "\n")
                  }
                  else if (period > 0)
                  {
                     # Range Variable
                     if (!is.na(match(ostring, allvars))) {
                        # 1. open is lagged to beginning of period
                        lperiod <- period - 1
                        ovar <- paste(ostring, lperiod, sep=".")
                        expr <- paste(expr1, ovar, " = splatr.shift(", ostring, ", ", -lperiod, "))", sep="")
                        cat("Open Variable: ", ovar, ", Expression: ", expr, "\n")
                        eval(parse(text = expr))
                        vexpr <- gsub(ostring, ovar, vexpr)
                     }
                     if (!is.na(match(hstring, allvars))) {
                        # 2. high is defined as highesthigh of period
                        func <- splatr.vparse(hhstring)[["func"]]
                        hhvar <- paste(hhstring, period, sep="")
                        expr <- paste(expr1, hhvar, " = splatr.vroll(", hstring, ", ", period, ", 1, ", func, fnopts, "))", sep="")
                        cat("High Variable: ", hhvar, ", Expression: ", expr, "\n")
                        eval(parse(text = expr))                  
                        vexpr <- gsub(hstring, hhvar, vexpr)
                     }
                     if (!is.na(match(lstring, allvars))) {
                        # 3. low is defined as lowestlow of period
                        func <- splatr.vparse(llstring)[["func"]]
                        llvar <- paste(llstring, period, sep="")
                        expr <- paste(expr1, llvar, " = splatr.vroll(", lstring, ", ", period, ", 1, ", func, fnopts, "))", sep="")
                        cat("Low Variable: ", llvar, ", Expression: ", expr, "\n")
                        eval(parse(text = expr))                  
                        vexpr <- gsub(lstring, llvar, vexpr)
                     }
                     # 4. close is always close, so we have the expression
                     expr <- paste(expr1, vxlag, " = ", vexpr, ")", sep="")
                     cat("Range Variable: ", vxlag, ", Expression: ", expr, "\n")
                  }
                  else
                  {
                     # Base Variable                     
                     expr <- paste(expr1, vroot, " = ", vexpr, ")", sep="")
                     cat("Base Variable: ", vroot, ", Expression: ", expr, "\n")
                  }
               }
               eval(parse(text = expr))
            }
            else if (lag > 0)
            {
               # Lag Variable
               expr <- paste(expr1, vname, " = splatr.shift(", vxlag, ", ", -lag, "))", sep="")
               cat("Lag Variable: ", vname, ", Expression: ", expr, "\n")
               eval(parse(text = expr))
            }
          } # end add loop
		} # end names loop
	} # end variables loop
}

splatr.vunapply <- function(g, gspace, vars)
{
   gnames <- tolower(splatr.gselect(g, "member"))
   glen <- length(gnames)
   for (i in seq(along=vars))
   {
      var <- vars[i]
      cat("Deleting variable", var, "from", glen, "frames:", gnames, "\n")
      for (j in seq(along=gnames))
      {
         fname <- splatr.getframename(gnames[j], g$subject, gspace)
         expr <- paste(fname, "$", var, "<<- NULL", sep="")
         eval(parse(text = expr))
     }
   }
}

# > vrapply(n, f, "cma1.50", 20)   # cma1.50.run20 "runs(cma1.50.run20)"

splatr.vrapply <- function(g, gspace, vars, runlength)
{
	newvars <- c()
	for (i in seq(along=vars))
	{
		vname <- paste(vars[i], ".run", runlength, sep="")
		vtext <- paste("runs (", vars[i], ")", sep="")
		splatr.newvariable(vname, vtext)
		newsvars <- c(newvars, vname)
	}
	splatr.vapply(g, gspace, newvars)
	newvars
}

