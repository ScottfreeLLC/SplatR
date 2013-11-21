##############################################################
#
# Package   : splatr
# Module    : states
# Version   : 1.0
# Copyright : RS Analytics LLC
# Date      : June 29, 2013
#
##############################################################

splatr.getglobalstates <- function(name, subject)
{
   gs <- paste(name, subject, "states", sep=".")
   splatr.getref(gs)
}

# states constructor

splatr.newstates <-
   function(name,
            subject,
            space)
   {
      states <- splatr.getglobalstates("splatr", space$wherein)
      fnew <- data.table()
      if (!is.null(fnew)) {
         fname <- splatr.getframename(name, subject, space)
         splatr.setframe(fname, fnew)
         fname
      }
   }

splatr.setgroupstates <- function(name, subject, space)
{
   gsname <- splatr.getframename(name, subject, space)
   gsframe <- splatr.getframe(gsname)
   if (is.null(gsframe))
      splatr.newstates(name, subject, space)
   gsname
}

splatr.object2state <- function(object, frame)
{
   oclass <- class(object)
   if (oclass == "list")
      object
   else {
      clist <- strsplit(oclass, "\\.")[[1]]
      sclass <- clist[[length(clist)]]
      states <- splatr.getglobalstates("splatr", sclass)
      snames <- names(states)
      for (i in seq(along = snames))
         states[[i]] <- object$field(snames[i])
      states
   }
}

splatr.setstate <- function(sfname, object)
{
   sframe <- splatr.getframe(sfname)
   state <- splatr.object2state(object, sframe)
   assign(sfname, rbind(sframe, state), envir = .GlobalEnv)
}

splatr.getstate <- function(so, keyvalue, name)
{
   kvc <- as.character(keyvalue)
   setkey(so, "date")
   with(so[kvc], get(name))
}
