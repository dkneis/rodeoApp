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
#' Plot state variables
#'
#' Plots the simulated dynamics of state variables.
#'
#' @param out Object returned from latest call to \code{simul}.
#' @param out_old Object returned from an earlier call to \code{simul}.
#' @param model Object of class \code{rodeo} representing the model.
#' @param mult Named vector with multipliers to apply to variables before plotting.
#' @param show Named logical vector to enable plotting of individual variables.
#' @param trange Range of the x-axis (time axis).
#' @param yrange Range of the y-axis.
#' @param logY Logical to switch between linear and log axis.
#' @param showOld Logical. Enables/disables plotting of data in \code{out_old}.
#'
#' @return \code{NULL}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
plotStates= function(out, out_old, model, mult, show, trange, yrange, logY, showOld) {
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
  times= out[,1]
  layout(matrix(1:2,ncol=2),widths=c(5,1))
  opar=par(c("mar", "cex"))
  par(mar=c(6,6,0.5,0.5), cex=1.25)
  plot(trange, yrange, type="n", log=ifelse(logY,"y",""),
    xlab="Time", ylab="State variable(s)")
  usr= par("usr")
  rect(xleft=usr[1], xright=usr[2], ybottom=ifelse(logY,10^usr[3],usr[3]),
    ytop=ifelse(logY,10^usr[4],usr[4]), col="grey85")
  # Previous output
  if ((!is.null(out_old)) && showOld) {
    times_old= out_old[,1]
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
    }
  }
  par(mar=opar$mar, cex=opar$cex)

  # Legend
  opar=par(c("mar", "cex"))
  par(mar=rep(0.1,4), cex=1.25)
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  inds= which(model$namesVars() %in% names(show)[show])
  legend("left",
    bty="n", seg.len=1.5,
    lty=inds, lwd=2, col=clr(inds),
    legend=model$namesVars()[inds])
  par(mar=opar$mar, cex=opar$cex)
  layout(matrix(1,ncol=1))
}


