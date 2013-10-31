# splatr class definitions

setClass("splatr",
	representation(name = "character",
					 subject = "character"),
	contains = "VIRTUAL")

# get global subject states

splatr.getglobalstates <- function(name, subject)
{
	gs <- paste(name, subject, "states", sep=".")
   splatr.getsmart(gs)
}

# define R vector functions

splatr.vectorfunctions <<-
   c("max",
     "min",
     "sum",
     "mean",
     "median",
     "range",
     "var",
     "cor",
     "sort",
     "rank",
     "order",
     "quantile",
     "cumsum",
     "cumprod",
     "cummax",
     "cummin",
     "pmax",
     "pmin",
     "colMeans",
     "colSums",
     "rowMeans",
     "rowSums")
