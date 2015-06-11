#' Run a simulation
#'
#' Computes the values of the state variables for a sequence of times.
#'
#' @param model Object of class \code{rodeo} representing the model.
#' @param vars Named vector of the state variables' initial values.
#' @param pars Named vector of parameters.
#' @param times Vector of times for which the states are computed.
#' @param dllfile Shared library file holding the compiled model.
#' @param rtol Vector of the same length as \code{vars} specifying the
#'   relative tolerance for the solver (see help of \code{deSolve::lsoda}).
#' @param atol Vector of the same length as \code{vars} specifying the
#'   absolute tolerance for the solver (see help of \code{deSolve::lsoda}).
#'
#' @return The object returned by \code{deSolve::ode}.
#'
#' @note An error is generated if the integration was not successful.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
simul= function(model, vars, pars, times, dllfile, rtol, atol) {
  # Transform input data
  vars= model$arrangeVars(as.list(vars))
  pars= model$arrangePars(as.list(pars))
  # Also arrange the tolerances
  rtol= model$arrangeVars(as.list(rtol))
  atol= model$arrangeVars(as.list(atol))
  # Integrate
  ext= substr(.Platform$dynlib.ext, 2, nchar(.Platform$dynlib.ext))
  dllname= sub(pattern=paste0("(.+)[.]",ext,"$"),replacement="\\1",
    x=basename(dllfile))
  dyn.load(dllfile)
  out= deSolve::ode(y=vars, times=times, func="derivs_wrapped", rtol=rtol, atol=atol, dllname=dllname,
    initfunc="initmod", nout=model$lenPros(), outnames=model$namesPros(), parms=pars, NLVL=1)
  if (attr(out,which="istate",exact=TRUE)[1] != 2)
    stop(paste0("Integration failed.\n----- The initial values were:\n",
      paste(names(vars),vars,sep="=",collapse="\n"),"\n----- The parameters were:\n",
      paste(names(pars),pars,sep="=",collapse="\n")
    ))
  dyn.unload(dllfile)
  # Return
  return(out)
}

