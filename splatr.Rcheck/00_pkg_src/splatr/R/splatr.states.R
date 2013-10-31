setClass("splatr.states",
	contains = "splatr",
	representation(
		frame = "splatr.frame",
		content = "character",		# class.states
		key = "character"),
	validity = splatr.validstates)

splatr.newstates <-
	function(name,
			  subject,
			  frame,
			  content,
			  key="name")
{
	value <- new("splatr.states",
				   name = name,
				   subject = subject,
				   frame = frame,
				   content = content,
				   key = key)
	if (!is.null(value))
		splatr.setframename(name, subject, frame, value)
	value
}

splatr.getstates <-
	function(group,
			  sframe = splatr.newframe("price", "states", "d1"))
{
	snames <- splatr.gselect(group, "member")
	for (i in seq(along = snames))
		splatr.getframe(snames[i], group@subject, sframe)
}

splatr.setgroupstates <- function(gname, gsubject, sclass, sframe)
{
	sobj <- splatr.getframe(gname, gsubject, sframe)
	if (!is.null(sobj))
		gstates <- splatr.newstates(gname, gsubject, sframe, splatr.getglobalstates(sclass))
	else
		gstates <- sobj
	gstates
}

splatr.setstate <- function(sframe, sobject, state, date)
{
	snames <- names(state)
	if (missing(date))
		date <- date()
	gstate <- splatr.getglobalstates(class(sobject))
	if (missing(state) & is.null(gstate))
		record <- unclass(sobject)		# record all slots of the object
	else {
		rstate <- ifelse(!missing(state), state, gstate)
		sapply(1:length(rstate),
				function(i)
				{
					svalue <- slot(sobject, snames[i])
					if (!is.na(svalue))
						rstate[[i]] <- svalue
				})
		record <- state
	}
	if (is.na(record[["date"]]))
		record <- c(list(date), record)
	sframe <- rbind(sframe, record)
	sframe
}

splatr.getstate <- function(so, keyvalue, slots, date, offset)
{
	key <- so@key
	if (missing(date()))
		date <- date()
	if (missing(offset))
		offset <- 0
	if (offset > 0) {
		subo <- so[which(date >= date & key == keyvalue),]
		index <- min(offset + 1, length(subo))
	}
	else {
		subo <- so[which(date <= date & key == keyvalue),]
		index <- max(1, length(subo) + offset)
	}
	subset(subo[index], select=slots)
}

splatr.trackstates <- function(frame, states)
{
	nr <- nrow(frame)
	nc <- length(states)
	snames <- names(states)
	sframe <- data.frame(nrow=nr, ncol=nc)
	names(sframe) <- snames
	frame <- data.frame(frame, sframe)
	for (i in seq(along=states))
	{
		sclass <- class(states[[i]])
		if (sclass == "expression")
			with(frame, assign(snames[i], eval(states[[i]])))
		else
			with(frame, assign(snames[i], rep(states[[i]], nr)))
	}
	rm(sframe)
	frame
}