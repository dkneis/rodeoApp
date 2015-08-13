################################################################################
#' Visualize the stoichiometry matrix
#'
#' Creates a graphical representation of the stoichiometry matrix.
#'
#' @param model Object of class \code{rodeo} representing the model.
#' @param vars Named vector of the state variables' initial values.
#' @param pars Named vector of parameters.
#' @param funsR File with function definitions as R code.
#'
#' @return Name of a temporary file containing generated HTML code.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
visStoi= function(model, vars, pars, funsR) {
  source(funsR)
  signsymbol= function(x) {
    if (as.numeric(x) > 0) return("<span style='color: #E09114;'>&Delta;</span>") #return("&#9651;")
    if (as.numeric(x) < 0) return("<span style='color: #5684C1;'>&nabla;</span>") #return("&#9661;")
    return("") #return("&empty;")
  }
  m= model$stoichiometryMatrix(c(vars, pars))
  tbl= cbind(data.frame(process=rownames(m), stringsAsFactors=FALSE), as.data.frame(m))
  html= exportDF(x=tbl, tex=FALSE,
    colnames= setNames(c("Process",model$getVars()$html[match(colnames(m),
      model$getVars()$name)]), names(tbl)),
    align= c(process="left",setNames(rep("center",ncol(m)), colnames(m))),
    funCell= setNames(replicate(ncol(m),signsymbol), colnames(m))
  )
  #tf= tempfile()
  #write(x=html, file=tf)
  #return(tf)
  return(html)
}

################################################################################
#' Find a reasonable time format
#'
#' Finds an appropriate time format specifier for a given time range
#'
#' @param tmin Lower limit of time axis (\code{POSIXct}).
#' @param tmax Upper limit of time axis (\code{POSIXct}).
#'
#' @return A format string.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}

timeFmt= function(tmin, tmax) {
  secrange= as.numeric(difftime(tmax, tmin, units="secs"))
  if (secrange > 86400*62) {
    fmt="%d %b '%y"
  } else if ((secrange > 86400*7) && (secrange <= 86400*62)) {
    fmt="%d %b"
  } else if ((secrange > 86400) && (secrange <= 86400*7)) {
    fmt="%d %b %Hh"
  } else {
    fmt="%H:%M:%S"
  }
  return(fmt)
}

################################################################################
#' Create a vector of times for labelling a time axis.
#'
#' This algorithm usually gives better results than axis.POSIXt.
#'
#' @param tmin Lower limit of time axis (\code{POSIXct}).
#' @param tmax Upper limit of time axis (\code{POSIXct}).
#' @param n Desired number of axis ticks.
#'
#' @return A list with components \code{values} and \code{format}. See example
#'   for usage.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @examples
#' \dontrun{
#'   times= prettyTimes(lower, upper, 10)
#'   axis.POSIXct(1, at=times$values, format=times$format, las=2, xlab="")
#' }

prettyTimes= function(tmin, tmax, n) {
  # Correct unreasonable n
  if (n < 2) n= 2
  # Years
  s= seq.POSIXt(from= tmin, to=tmax, by="year")
  k= length(s) 
  if (k >= n) {
    i= 1:n * ceiling(k/n)
    t1= ISOdatetime(format(s[1],"%Y"),1,1,0,0,0)
    t2= ISOdatetime(format(s[k],"%Y"),1,1,0,0,0)
    x= seq.POSIXt(from=t1, to=t2, by="year")
    return(list(values=x[i[i<=k]], format=timeFmt(tmin,tmax)))
  }
  # Months
  s= seq.POSIXt(from= tmin, to=tmax, by="month")
  k= length(s) 
  if (k >= n) {
    i= 1:n * ceiling(k/n)
    t1= ISOdatetime(format(s[1],"%Y"),format(s[1],"%m"),1,0,0,0)
    t2= ISOdatetime(format(s[k],"%Y"),format(s[k],"%m"),1,0,0,0)
    x= seq.POSIXt(from=t1, to=t2, by="month")
    return(list(values=x[i[i<=k]], format=timeFmt(tmin,tmax)))
  }
  # Days
  s= seq.POSIXt(from= tmin, to=tmax, by="day")
  k= length(s) 
  if (k >= n) {
    i= 1:n * ceiling(k/n)
    t1= ISOdatetime(format(s[1],"%Y"),format(s[1],"%m"),format(s[1],"%d"),0,0,0)
    t2= ISOdatetime(format(s[k],"%Y"),format(s[k],"%m"),format(s[k],"%d"),0,0,0)
    x= seq.POSIXt(from=t1, to=t2, by="day")
    return(list(values=x[i[i<=k]], format=timeFmt(tmin,tmax)))
  }
  # Hours
  s= seq.POSIXt(from= tmin, to=tmax, by="hour")
  k= length(s) 
  if (k >= n) {
    i= 1:n * ceiling(k/n)
    t1= ISOdatetime(format(s[1],"%Y"),format(s[1],"%m"),format(s[1],"%d"),format(s[1],"%H"),0,0)
    t2= ISOdatetime(format(s[k],"%Y"),format(s[k],"%m"),format(s[k],"%d"),format(s[k],"%H"),0,0)
    x= seq.POSIXt(from=t1, to=t2, by="hour")
    return(list(values=x[i[i<=k]], format=timeFmt(tmin,tmax)))
  }
  # Minutes
  s= seq.POSIXt(from= tmin, to=tmax, by="min")
  k= length(s) 
  if (k >= n) {
    i= 1:n * ceiling(k/n)
    t1= ISOdatetime(format(s[1],"%Y"),format(s[1],"%m"),format(s[1],"%d"),format(s[1],"%H"),format(s[1],"%M"),0)
    t2= ISOdatetime(format(s[k],"%Y"),format(s[k],"%m"),format(s[k],"%d"),format(s[k],"%H"),format(s[k],"%M"),0)
    x= seq.POSIXt(from=t1, to=t2, by="min")
    return(list(values=x[i[i<=k]], format=timeFmt(tmin,tmax)))
  }
  # Seconds
  x= seq.POSIXt(from= tmin, to=tmax, length.out=n)
  return(list(values=x, format=timeFmt(tmin,tmax)))
}

################################################################################
#' Plot state variables
#'
#' Plots the simulated dynamics of state variables.
#'
#' @param out Object returned from latest call to \code{simul}.
#' @param out_old Object returned from an earlier call to \code{simul}.
#' @param timeUnit Unit of numeric simulation time info (as a string).
#' @param timeBase Date and time of the startpoint of the simulation as a
#'   value of class \code{POSIXct}.
#' @param model Object of class \code{rodeo} representing the model.
#' @param mult Named vector with multipliers to apply to variables before plotting.
#' @param show Named logical vector to enable plotting of individual variables.
#' @param rangeT Range of the x-axis (time axis).
#' @param rangeY Range of the y-axis.
#' @param gridT Logical to enable/disable grid for x-axis (time axis).
#' @param gridY Logical to enable/disable grid for y-axis.
#' @param logY Logical to switch between linear and log axis.
#' @param labelY Y-axis label.
#' @param showOld Logical. Enables/disables plotting of data in \code{out_old}.
#' @param obs \code{NULL} or a data frame with observations (numeric time info
#'   is expected in the first column).
#'
#' @return \code{NULL}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
plotStates= function(out, out_old, timeUnit, timeBase,
  model, mult, show, rangeT, rangeY,
  gridT, gridY, logY, labelY, showOld, obs) {
  clrHelp= colorRamp(c("violetred4","orangered2","indianred",
    "darkseagreen","dodgerblue3","blue4"), space="rgb")
  clr= function(i) {
    if ("color" %in% names(model$getVars())) {
      res= model$getVars()[i,"color"]
    } else {
      res= rgb(clrHelp((i-1)/(model$lenVars()-1)), maxColorValue=255)
    }
    return(res)
  }
  # Convert times
  timeMult= c(seconds=1, hours=3600, days=86400)
  if (!(timeUnit %in% names(timeMult)))
    stop(paste0("time unit '",timeUnit,"' not supported"))
  timeBase= as.POSIXct(strptime(timeBase,"%Y-%m-%dT%H:%M:%S"))
  if (!is.finite(timeBase))
    stop(paste0("base time '",timeBase,"' invalid of mal-formatted"))
  times= timeBase + (out[,1] - out[1,1]) * timeMult[timeUnit]
  rangeT= timeBase + (rangeT - out[1,1]) * timeMult[timeUnit]
  layout(matrix(1:2,ncol=2),widths=c(5,1))
  opar=par(c("mar", "cex"))
  par(mar=c(6,4.1,1.5,0.5), cex=1.25)
  plot(rangeT, rangeY, type="n", log=ifelse(logY,"y",""), xaxt="n",
    xlab="", ylab=labelY)
  timeTicks= prettyTimes(rangeT[1], rangeT[2], 10)
  axis.POSIXct(side=1, at=timeTicks$values, format=timeTicks$format, las=2)
  usr= par("usr")
  rect(xleft=usr[1], xright=usr[2], ybottom=ifelse(logY,10^usr[3],usr[3]),
    ytop=ifelse(logY,10^usr[4],usr[4]), col="grey90")
  if (gridT)
    abline(v=timeTicks$values, col="grey60")
  if (gridY)
    grid(nx=NA, ny=NULL, col="grey60")
  # Previous output
  if ((!is.null(out_old)) && showOld) {
    times_old= timeBase + (out_old[,1] - out_old[1,1]) * timeMult[timeUnit]
    for (i in 1:model$lenVars()) {
      varname= model$namesVars()[i]
      if (show[varname]) {
        lines(times_old, out_old[,1+i]*mult[varname], lty=i, lwd=2, col="white")
      }
    }
  }
  # Latest output
  for (i in 1:model$lenVars()) {
    varname= model$namesVars()[i]
    if (show[varname]) {
      lines(times, out[,1+i]*mult[varname], lty=i, lwd=2, col=clr(i))
      if ((!is.null(obs)) && (varname %in% names(obs))) {
        points(obs[,1], obs[,varname]*mult[varname], pch=i, col=clr(i))
      }
    }
  }
  par(mar=opar$mar, cex=opar$cex)

  # Legend
  opar=par(c("mar", "cex"))
  par(mar=c(.1, .1, .1, .2), cex=1.25)
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  inds= which(model$namesVars() %in% names(show)[show])
  legend("left",
    bty="n", seg.len=1.5,
    lty=inds, lwd=2, col=clr(inds),
    legend=model$namesVars()[inds])
  par(mar=opar$mar, cex=opar$cex)
  layout(matrix(1,ncol=1))
}


