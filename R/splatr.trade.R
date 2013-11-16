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