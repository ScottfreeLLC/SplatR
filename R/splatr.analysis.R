##############################################################
#
# Package   : splatr
# Module    : analysis
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.newanalysis <-
	function(group,
			   output = "highsep",
			   inputs = ".",
			   method = splatr.lrmethod,
			   lag = 1,
			   groupby = "month",
	         sspace = splatr.newspace("price", "states", "d1"),
            runcv = FALSE,
	         runwf = FALSE,
	         cvparams = 0,
            wfparams = c(2, 2),
	         ...)
{
	group$space <- sspace
	aspace <- splatr.newspace(output, "analysis", sspace$fractal)
	newa <- new("splatr.analysis",
	            group = group,
	            output = output,
	            inputs = inputs,
	            method = method,
	            lag = lag,
	            space = aspace,
	            groupby = groupby,
               cvparams = cvparams,
               wfparams = wfparams,
               models = list(),
               results = data.table(),
	            cvparams = cvparams,
	            cverror = 0.0,
	            wfparams = wfparams,
	            wferror = 0.0)
   if (!is.null(newa)) {
      newa$name <- splatr.getframename(group$name, group$subject, aspace)
	   splatr.setframe(newa$name, newa)
	   group$gfn <- splatr.analyze
	   splatr.gapply(group, newa, runcv, runwf, ...)
	}
   newa$name
}

# > makefitfn("glm", "type", ".", "binomial")
#
# --> function (d, x) glm(type ~ ., data=d[x,], binomial)

splatr.makefitfn <- function(inputfn, output, inputs, options)
{
   vlist <- paste(inputs, sep = "", collapse = " + ")
   fstring <- paste("function (d, x) ", inputfn, "(", output, " ~ ", vlist,
                    ", data=d[x,], ", options, ", na.action=na.omit)", sep = "")
   eval(parse(text=fstring))
}

# > makepredfn("predict", "type=\"output\"")
#
# function (m, d, x) predict(m, data=d[x,], type="output")

splatr.makepredfn <- function(outputfn, options)
{
   fstring <- paste("function (m, d, x) ", outputfn,
                    "(m, data=d[x,], ", options, ")", sep = "")
   eval(parse(text=fstring))
}

splatr.geterrorrate <- function(...)
{
   tab <- table(...)
   diag(tab) <- 0
   print(round(100 * sum(tab) / length(list(...)[[1]]), 2))
}

splatr.runcv <-
   function(data,
            output,
            folds,
            fitfn,
            predfn,
            cvparams,
            ...)
{
   resp <- with(data, get(output))
   pred <- resp
   for (i in sort(unique(folds)))
   {
      suppressWarnings(learn <- fitfn(data, folds != i, ...))
      suppressWarnings(pred[folds == i] <- predfn(learn, data, folds == i))
   }
   splatr.geterrorrate(true = resp, predicted = pred)
}

splatr.runwf <-
   function(data,
            output,
            folds,
            fitfn,
            predfn,
            wfparams)
{
   resp <- with(data, get(output))
   pred <- resp
   nt <- wfparams[1]
   if (is.na(nt)) nt <- 1
   nv <- wfparams[2]
   if (is.na(nv)) nv <- 1
   nfolds <- length(folds)
   svec <- numeric(nfolds)
   seqt <- 0:(nt-1)
   seqv <- nt:(nt + nv - 1)
   for (i in sort(unique(folds)))
   {
      seqt <- (seqt + 1) %% nfolds
      seqt[seqt == 0] <- nfolds
      seqv <- (seqv + 1) %% nfolds
      seqv[seqv == 0] <- nfolds
      ft <- (folds == seqt)
      fv <- (folds == seqv)
      suppressWarnings(learn <- fitfn(data, ft))
      suppressWarnings(pred[ft] <- predfn(learn, data, fv))
   }
   splatr.geterrorrate(true = resp, predicted = pred)
}

# The main analysis routine

splatr.analyze <- function(a, name, runcv, runwf)
{
   cat("Analyzing:", name, "\n")
	g <- a$group
	output <- a$output
	inputs <- a$inputs
	method <- a$method
	lag <- a$lag
	groupby <- a$groupby
   gspace <- g$space
   stats <- method$stats
   nstats <- length(stats)
   # locate the data frame
   sname <- splatr.getframename(name, g$subject, gspace)
   states <- splatr.getframe(sname)
   # collect all the variables for the model
	allvars <- names(states)
	stopifnot(output %in% allvars)
   if (!is.na(match(".", inputs)))
      mvars <- allvars
   else
      mvars <- inputs[!is.na(match(inputs, allvars))]
   # see if we need to add any to the frame not already there
   mvars <- unique(mvars)
   avars <- mvars[!(mvars %in% allvars)]
   splatr.vapply(g, gspace, avars)
	# lag all apriori variables
	expr1 <- paste(sname, " <<- transform(", sname, ", ", sep = "")
	for (i in seq(along = mvars)) {
      svar <- mvars[i]
      isnotlead <- is.na(match(svar, splatr.lead.variables))
      if (isnotlead) {
         svar.1 <- paste(svar, lag, sep = ".")
         mvars[match(svar, mvars)] <- svar.1
         if (is.na(match(svar.1, allvars))) {
            expr <- paste(expr1, svar.1, " = splatr.shift(", svar, ", ", -lag, "))", sep="")
            eval(parse(text = expr))               
         }
      }
   }
   # construct the fitting function for model generation
   vlist <- paste(mvars, sep = "", collapse = " + ")
   ifname <- method$ifname
   ifopt <- method$ifopt
   ofname <- method$ofname
   ofopt <- method$ofopt
   fitfn <- splatr.makefitfn(ifname, output, vlist, ifopt)
   # run the method
   models <- splatr.runmethod(sname,
									   groupby,
									   fitfn)
   # allocate the results data frame
   ndim <- 3
   nvars <- length(mvars)
   folds <- as.integer(with(states, get(groupby)))
   foldids <- sort(unique(folds))
   nfolds <- length(foldids)
   totalrows <- nvars * nfolds
   results <- as.data.table(matrix(nrow = totalrows, ncol = nstats + ndim))
   results[, 1] <- rep(name, totalrows)
   results[, 2] <- rep(mvars, each = nfolds)
   results[, 3] <- rep(seq(along = foldids), nvars)
   statnames <- unlist(sapply(seq(along=stats), function (i) stats[[i]]$sname))
   statlen <- length(statnames)
   setnames(results, 1:(statlen + ndim), c("name", "variable", groupby, statnames))
   # process the results
   if (nstats > 0) {
      for (i in seq(along = stats))
      {
         stat <- stats[[i]]
         sofn <- stat$outputfn
         results[, ndim + i] <- splatr.getstat(stat, mvars, models) # sofn
      }      
   }
   # construct the prediction function for model testing
   predfn <- splatr.makepredfn(ofname, ofopt)
   # run the cross-validation test
   if (runcv)
      a$cverror <- splatr.runcv(states,
                                output,
                                folds,
                                fitfn,
                                predfn,
                                a$cvparams)
   # run the walk-forward test
   if (runwf)
      a$wferror <- splatr.runwf(states,
                                output,
                                folds,
                                fitfn,
                                predfn,
                                a$wfparams)
   # append the results
   a$models <- c(a$models, models)
   a$results <- rbind(a$results, results)
   # end of the SplatR workhorse
   return(a)
}

# Go through permutations of Name-Variable-Fold (NVF)

splatr.summarya <- function(a, afn = mean)
{
   aname <- a$name
   g <- a$group
   output <- a$output
   inputs <- a$inputs
   method <- a$method
   lag <- a$lag
   space <- a$space
   groupby <- a$groupby
   results <- na.omit(a$results)
   stats <- method$stats
   fname <- deparse(substitute(afn))
   # Loop through all permutations of name, variable, fold
	cat("\nEdge Analysis for:", splatr.getgroupname(g$name, g$subject), "\n")
   n <- 3
	efac <- colnames(results[1:n])
	r <- 1
	x <- numeric(n + 1)
	repeat {
		if (x[r] < n) {
			x[r+1] <- x[r] + 1
			r <- r + 1
		}
		else {
			r <- r - 1
			x[r] <- x[r] + 1
		}
		seqg <- seq(length = r-1, x[2])
		efacs <- efac[seqg]
      bystring <- sapply(seq(along=efacs),
                         function(i) gsub(efacs[i], paste(efacs[i], "=", efacs[i]), efacs[i]))
      bystring <- paste("list(", paste(bystring, collapse = ", "), ")", sep="")
		cat("\nCategories: ", r - 1, "(", efacs, ")\n")
		for (i in seq(along = stats))
		{
		   stat <- stats[[i]]
         sname <- stat$sname
         desc <- stat$desc
		   cuts <- stat$cuts
		   cutlabels <- stat$cutlabels
		   # aggregate for this statistic
         sresults <- NULL
         sindex <- i + n
		   aggexpr <- paste("with(results, aggregate(results[ ,", sindex,
                          "], by = ", bystring, ", ", fname, "))", sep="")
		   aggres <- eval(parse(text = aggexpr))
         sresults <- aggres
         ncols <- ncol(aggres)
         setnames(sresults, ncols, sname)
         # now apply the range values to the statistic
         slevels <- cutlabels[cut(sresults[,ncols], breaks=cuts, labels=FALSE)]
		   sresults <- cbind(sresults, slevels)
         setnames(sresults, ncols + 1, desc)
		   # count the number of cases for each tuple
		   texpr <- paste("as.vector(with(results, table(", bystring, ")))")
		   tcounts <- eval(parse(text = texpr))
         tcounts <- tcounts[which(tcounts != 0)]
		   sresults <- cbind(sresults, tcounts)
         setnames(sresults, ncols + 2, "cases")
		   # give the overall picture of the edge for this stat
		   cat("\nStatistic:", sname, "\n\n")
         print(sresults)
		   # cat("    ", format(p), "% (", subtotals[i], "/", total, ") of the cases are", pdesc[i], "\n")
		   # cat("\n    ", format(p2), "% of the cases are at least", pdesc[2], "\n")}
		}
		if (seqg[1] == n) break
	}
}