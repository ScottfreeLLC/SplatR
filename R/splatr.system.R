##############################################################
#
# Package   : splatr
# Module    : system
# Version   : 1.0
# Copyright : Splatr LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getsystemname <- function(name)
{
   sname <- paste(name, "system", sep=".")
   sname
}

splatr.setsystem <- function(sname, sobject)
{
   assign(sname, sobject, envir = .GlobalEnv)
   sname
}

splatr.getsystem <- function(sname)
{
   splatr.getref(sname)
}

splatr.newsystem <-
   function(name,
            longevent = "higherclose",
            shortevent = "lowerclose",
            holdperiod = 0,
            scale = FALSE)
{
   news <- new("splatr.system",
               name = name,
               subject = "system",
               longevent = longevent,
               shortevent = shortevent,
               holdperiod = holdperiod,
               scale = scale)
   if (!is.null(news)) {
      sname <- splatr.getsystemname(name)
      splatr.setsystem(sname, news)      
   }
   else ""
}

splatr.updatesystemstate <-
   function(system,
            sfname,
            sstate,
            tfname,
            trade)
{
   sframe <- splatr.getref(sfname)
   tframe <- splatr.getref(tfname)
   #
   # set the stop/limit prices and when they are triggered
   #
   event <- paste(system$longevent, collapse = " & ")
   longevent <- with(sframe, eval(parse(text = event)))   
   event <- paste(system$shortevent, collapse = " & ")
   shortevent <- with(sframe, eval(parse(text = event)))   
   # extract the system parameters
   holdperiod <- system$holdperiod
   scale <- system$scale
   # this is where the magic happens
   le <- "le"
   se <- "se"
   lx <- "lx"
   sx <- "sx"
   lt <- "lt"
   st <- "st"
   lets <- which(longevent, TRUE)
   sets <- which(shortevent, TRUE)
   ltable <- data.table(ebar = lets, entry = le, xbar = 0, exit = NA, key = "ebar")
   stable <- data.table(ebar = sets, entry = se, xbar = 0, exit = NA, key = "ebar")
   ttable <- rbind(ltable, stable)
   ttable <- ttable[order(ebar), ]
   # insert holding period trades
   if (holdperiod > 0)
   {
      ttable[,xbar := ifelse(c(pmax(diff(ebar) - holdperiod, 0), 0) > 0, ebar + holdperiod, 0)]
      ttable[,exit := ifelse(entry == le, lx, sx)]
   }
   # generate trade file
   p <- 0
   q <- trade[["quantity"]]
   tf <- data.table()
   for (i in seq(nrow(ttable)))
   {
      t <- ttable[i]
      longentry <- t$entry == le
      longexit <- t$exit == lx
      # entries
      trade[["date"]] <- splatr.chrondate(sframe$date[t$ebar])
      trade[["price"]] <- sframe$close[t$ebar]
      if (longentry)
      {
         if (p < 0) {
            # short active
            trade[["order"]] <- sx
            trade[["quantity"]] <- -p
            tf <- rbind(tf, trade)
            p <- 0
         }
         if (p == 0 | scale) {
            trade[["order"]] <- le
            trade[["quantity"]] <- q
            tf <- rbind(tf, trade)
            p <- p + q
         }
      }
      else
      {
         if (p > 0) {
            # long active
            trade[["order"]] <- lx
            trade[["quantity"]] <- -p
            tf <- rbind(tf, trade)
            p <- 0
         }
         if (p == 0 | scale) {
            trade[["order"]] <- se
            trade[["quantity"]] <- -q
            tf <- rbind(tf, trade)            
            p <- p - q
         }
      }
      # exits
      if (t$xbar > 0) {
         trade[["date"]] <- splatr.chrondate(sframe$date[t$xbar])
         trade[["price"]] <- sframe$close[t$xbar]
         if (longexit) {
            trade[["order"]] <- lt
            trade[["quantity"]] <- -p
         }
         else {
            trade[["order"]] <- st
            trade[["quantity"]] <- -p
         }
         tf <- rbind(tf, trade)            
         p <- 0
      }
   }
   assign(tfname, tf, envir = .GlobalEnv)
}

splatr.runsystem <-
   function(group,
            system,
            sspace = splatr.newspace("price", "states", "d1"),
            startcap = 100000,
            quantity = 1000,
            posby = "close",
            bperiod = 0,
            ...)
{
   gname <- group$name
   subject <- group$subject
   gmembers <- splatr.gselect(group, "member")
   glen <- length(gmembers)
   # allocate a trade object
   tstate <- splatr.getglobalstates("splatr", "trade")
   tstate[["quantity"]] <- quantity
   # extract the group information
   # now run the system for each group member
   sname <- splatr.getsystemname(system)
   s <- splatr.getsystem(sname)
   # set up the trades frame
   tspace <- splatr.newspace(system, "trade", sspace$fractal)
   # run the system for each member of the group
   for (i in seq(along=gmembers))
   {
      mname <- gmembers[i]
      tstate[["name"]] <- mname
      # states data
      sfname <- splatr.getframename(mname, subject, sspace)
      # trade data
      tfname <- splatr.getframename(mname, subject, tspace)
      # update state and trade files
      splatr.updatesystemstate(s, sfname, sstate, tfname, tstate)
   }
   # create group trades file
   stspace <- splatr.newspace(system, "trade", tspace$fractal)
   stfname <- splatr.getframename(gname, group$subject, stspace)
   stframe <- splatr.newstates(gname, group$subject, stspace)
   group$space <- tspace
   splatr.mergeframes(group, stfname)
   # generate the portfolio
   pname <- splatr.generateportfolio(group,
                                     stfname,
                                     startcap,
                                     posby,
                                     bperiod)
   # free memory
   rm(sspace, tspace, stspace)
   # return the portfolio generated by the systems
   return(pname)
}