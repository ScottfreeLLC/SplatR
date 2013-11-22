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
            order,
            quantity = 0,
            price = 0.0,
            tdate)
{
   if (missing(tdate))
      tdate <- date()
   nt <- new("splatr.trade",
             name = name,
             order = order,
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
            order,
            quantity,
            price,
            tdate)
{
   # access the portfolio and any position by name first
   portfolio <- splatr.getportfolio(portname)
   posname <- splatr.getpositionname(portname, name)
   position <- splatr.getposition(posname)   
   # check the dynamic position sizing variable
   psvar <- portfolio$posby
   psflag <- is.null(psvar)
   if (psflag)
      psize <- quantity
   else {
      if (order == "le" | order == "se") {
         pfname <- splatr.getframename(name, portfolio$subject, portfolio$space)
         pframe <- splatr.getframe(pfname)
         cv <- splatr.getstate(pframe, tdate, psvar)
         psize <- (portfolio$value * portfolio$fixedfrac) / cv
         if (quantity < 0)
            psize <- -psize         
      }
      else
         psize <- -position$quantity
   }
   # instantiate the trade, creating a new position if necessary
   newtrade <- splatr.newtrade(name = name,
                               order = order,
                               quantity = psize,
                               price = price,
                               tdate = tdate)
   newpos <- is.null(position)
   if (newpos) {
      splatr.newposition(portname,
                         name,
                         tdate)
      position <- splatr.getposition(posname)
   }
   # determine the amount that can be allocated
   allocation <- splatr.allocatetrade(portfolio,
                                      position,
                                      newtrade)
   if (allocation != 0)
   {
      if (newpos) {
         position$openprice <- price
         position$openvalue <- allocation * position$multiplier * position$openprice
         splatr.addportfolio(portfolio,
                             posname)
         portfolio$npos <- portfolio$npos + 1
      }
      splatr.updateportfolio(portfolio,
                             position,
                             newtrade,
                             allocation)
      # if net position is zero, then close the position
      pflat <- position$quantity == 0
      if (pflat) {
         splatr.closeposition(portfolio, position, tdate)
         portfolio$npos <- portfolio$npos - 1         
      }
      # record the portfolio and position states
      splatr.setstate(portfolio$gsname, portfolio)
      splatr.setstate(portfolio$msname, position)
      # remove the position from the portfolio
      if (pflat)
         splatr.removeportfolio(portfolio, posname)
   }
}

splatr.maketrades <-
   function(pname,
            tfname,
            bperiod)
{
   bflag <- ifelse(bperiod == 0, FALSE, TRUE)
   ntrades <- 0
   tframe <- splatr.getref(tfname)
   for (i in 1:nrow(tframe))
   {
      splatr.trade(pname,
                   tframe[i]$name,
                   tframe[i]$order,
                   tframe[i]$quantity,
                   tframe[i]$price,
                   tframe[i]$date)
      ntrades <- ntrades + 1
      if (bflag)
         if (ntrades %% bperiod == 0)
            splatr.balance(portfolio, tframe[i]$date)
   }
}