#' Run a dynamic simulation
#'
#' Computes the values of the state variables for a sequence of times.
#'
#' @param model Object of class \code{rodeo} representing the model.
#' @param vars Named vector of the state variables' initial values.
#' @param pars Named vector of parameters.
#' @param times Vector of times for which the states are computed.
#' @param dllfile Shared library file holding the compiled model.
#'
#' @return The object returned by \code{deSolve::ode}.
#'
#' @note An error is generated if the integration was not successful.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
simul <- function(model, vars, pars, times, dllfile) {
  # Assign data
  model$setVars(vars)
  model$setPars(pars)
  # Set tolerances
  rtol <- model$getVarsTable()$rtol
  atol <- model$getVarsTable()$atol
  # Load library
  ext <- substr(.Platform$dynlib.ext, 2, nchar(.Platform$dynlib.ext))
  dllname <- sub(pattern=paste0("(.+)[.]",ext,"$"),replacement="\\1",
    x=basename(dllfile))
  dyn.load(dllfile)
  # Integrate
  out <- deSolve::ode(y=model$getVars(), times=times, func="derivs_wrapped",
    rtol=rtol, atol=atol, dllname=dllname,
    initfunc="initmod", nout=model$lenPros(), outnames=model$namesPros(),
    parms=model$getPars())
  if (attr(out,which="istate",exact=TRUE)[1] != 2)
    stop(paste0("Integration failed.\n----- The initial values were:\n",
      paste(names(vars),vars,sep="=",collapse="\n"),"\n----- The parameters were:\n",
      paste(names(pars),pars,sep="=",collapse="\n")
    ))
  # Clean up and return
  dyn.unload(dllfile)
  return(out)
}

#' Compute steady-state solution
#'
#' Estimates the values of the state variables for steady-state conditions.
#'
#' @param time Scalar numeric value. All external forcings used in the steady
#'   state computations are kept constant at their respective values for the
#'   given point in time. If no external forcings are present (autonomous
#'   models), the value is ignored.
#'
#' @inheritParams simul
#'
#' @return The object returned by \code{rootSolve::steady}. The \code{y}-
#'   component of this object has names based on the \code{ynames}
#'   attribute.
#'
#' @note An error is generated if steady-state estimation was not successful.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
stst <- function(model, vars, pars, time, dllfile) {
  # Assign data
  model$setVars(vars)
  model$setPars(pars)
  # Set tolerances
  rtol <- model$getVarsTable()$rtol
  atol <- model$getVarsTable()$atol
  # Load library
  ext <- substr(.Platform$dynlib.ext, 2, nchar(.Platform$dynlib.ext))
  dllname <- sub(pattern=paste0("(.+)[.]",ext,"$"),replacement="\\1",
    x=basename(dllfile))
  dyn.load(dllfile)
  # Compute steady state solution
  out <- rootSolve::steady(y=model$getVars(), time=time, func="derivs_wrapped",
    parms=model$getPars(), method="stode", rtol=rtol, atol=atol,
    dllname=dllname, initfunc="initmod",
    nout=model$lenPros(), outnames=model$namesPros())
  if (!attr(out, which="steady",exact=TRUE))
    stop(paste0("Steady-state estimation failed.\n----- The initial values were:\n",
      paste(names(vars),vars,sep="=",collapse="\n"),"\n----- The parameters were:\n",
      paste(names(pars),pars,sep="=",collapse="\n")
    ))
  names(out$y)= attr(out, which="ynames",exact=TRUE)
  # Clean up and return
  dyn.unload(dllfile)
  return(out)
}

