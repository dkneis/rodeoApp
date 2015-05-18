################################################################################
#' Plot the stoichiometry matrix
#'
#' Creates a graphical representation of the stoichiometry matrix.
#'
#' @param model Object of class \code{rodeo} representing the model.
#' @param vars Named vector of the state variables' initial values.
#' @param pars Named vector of parameters.
#' @param funsR File with function definitions as R code.
#'
#' @return \code{NULL}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
plotStoi= function(model, vars, pars, funsR) {
  source(funsR)
  model$plot(c(vars, pars))
  return(NULL)
}

################################################################################
#' Plot state variables
#'
#' Plots the simulated dynamics of state variables.
#'
#' @param out Object returned from latest call to \code{simul}.
#' @param out_old Object returned from an earlier call to \code{simul}.
#' @param model Object of class \code{rodeo} representing the model.
#' @param yrange Range of the y-axis.
#' @param showOld Logical. Enables/disables plotting of data in \code{out_old}.
#'
#' @return \code{NULL}.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
plotStates= function(out, out_old, model, yrange, showOld) {
  clrHelp= colorRamp(c("violetred4","orangered2","indianred",
    "darkseagreen","dodgerblue3","blue4"), space="rgb")
  clr= function(i) {
    rgb(clrHelp((i-1)/(model$lenVars()-1)), maxColorValue=255)
  }
  times= out[,1]
  layout(matrix(1:2,ncol=2),widths=c(8,2))
  plot(range(times), yrange, type="n", log="y",
    xlab="Time", ylab="State variable(s)")
  usr= par("usr")
  rect(xleft=usr[1], xright=usr[2], ybottom=10^usr[3], ytop=10^usr[4], col="lightgrey")
  # Previous output
  if ((!is.null(out_old)) && showOld) {
    times_old= out_old[,1]
    for (i in 1:model$lenVars()) {
      lines(times_old, out_old[,1+i], lty=i, lwd=2, col="white") 
    }
  }
  # Latest output
  for (i in 1:model$lenVars()) {
    lines(times, out[,1+i], lty=i, lwd=2, col=clr(i)) 
  }
  # Legend
  omar=par("mar")
  par(mar=rep(0.1,4))
  plot(0,0,bty="n",type="n",xaxt="n",yaxt="n",xlab="",ylab="")
  legend("left",
    bty="n", seg.len=1.5,
    lty=1:model$lenVars(), lwd=2, col=clr(1:model$lenVars()),
    legend=model$namesVars())
  par(mar=omar)
  layout(matrix(1,ncol=1))
}

