##############################################################
#
# Package   : splatr
# Module    : trade
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.newtrade <-
   function(name,
            quantity = 0,
            price = 0.0,
            tdate)
{
   if (missing(tdate))
      tdate <- date()
   nt <- new("splatr.trade",
             name = name,
             quantity = quantity,
             price = price,
             tdate = tdate)
   nt
}

splatr.allocatetrade <-
   function(portfolio,
            position,
            trade)
{
   cash <- portfolio$cash
   margin <- portfolio$margin
   mincash <- portfolio$mincash
   restricted <- portfolio$restricted
   if (restricted)
   {
      splatr.kickout(portfolio, trade$tdate)
      splatr.stoploss(portfolio, trade$tdate)
   }
   multiplier <- position$multiplier
   qpold <- position$quantity
   qtrade <- trade$quantity
   qpnew <- qpold + qtrade
   allocation <- abs(qpnew) - abs(qpold)
   addedvalue <- trade$price * multiplier * abs(allocation)
   if (restricted)
   {
      cashreserve <- mincash * cash
      freemargin <- (cash - cashreserve) / margin
      if (addedvalue > freemargin) {
         cat("Required free margin:", freemargin, "< added value:", addedvalue, "\n")
         allocation <- 0
      }
      else {
         freecash <- cash - addedvalue
         if (freecash < 0)
            portfolio$cash <- cash + freecash
      }
   }
   allocation
}

splatr.trade <-
   function(portname,
            name,
            quantity,
            price,
            tdate)
{
   newtrade <- splatr.newtrade(name = name,
                               quantity = quantity,
                               price = price,
                               tdate = tdate)
   portfolio <- splatr.getportfolio(portname)
   pname <- splatr.getpositionname(portname, name)
   position <- splatr.getposition(pname)
   newpos <- is.null(position)
   if (newpos) {
      splatr.newposition(portname,
                         name,
                         tdate)
      position <- splatr.getposition(pname)
   }
   allocation <- splatr.allocatetrade(portfolio,
                                      position,
                                      newtrade)
   if (allocation != 0)
   {
      if (newpos) {
         splatr.addportfolio(portfolio,
                             pname)
         portfolio$npos <- portfolio$npos + 1
      }
      splatr.updateportfolio(portfolio,
                             position,
                             newtrade,
                             allocation)
   }
}

splatr.maketrades <-
   function(pname,
            tfname,
            bperiod)
{
   portfolio <- splatr.getportfolio(pname)
   psvar <- portfolio$posby
   psflag <- is.null(psvar)
   bflag <- ifelse(bperiod == 0, FALSE, TRUE)
   ntrades <- 0
   tframe <- splatr.getref(tfname)
   for (i in 1:nrow(tframe))
   {
      if (psflag)
         psize <- tframe[i]$quantity
      else {
         pfname <- splatr.getframename(tframe[i]$name, portfolio$subject, portfolio$space)
         pframe <- splatr.getframe(pfname)
         cv <- splatr.getstate(pframe, tframe[i]$date, psvar)
         psize <- (portfolio$value * portfolio$fixedfrac) / cv
      }
      splatr.trade(pname,
                   tframe[i]$name,
                   psize,
                   tframe[i]$price,
                   tframe[i]$date)
      ntrades <- ntrades + 1
      if (bflag)
         if (ntrades %% bperiod == 0)
            splatr.balance(portfolio, tframe[i]$date)
   }
}