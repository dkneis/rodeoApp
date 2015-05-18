#' Run a simulation
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
simul= function(model, vars, pars, times, dllfile) {
  # Transform input data
  vars= model$arrangeVars(as.list(vars))
  pars= model$arrangePars(as.list(pars))
  # Integrate
  ext= substr(.Platform$dynlib.ext, 2, nchar(.Platform$dynlib.ext))
  dllname= sub(pattern=paste0("(.+)[.]",ext,"$"),replacement="\\1",
    x=basename(dllfile))
  dyn.load(dllfile)
  out= deSolve::ode(y=vars, times=times, func="derivs_wrapped", dllname=dllname,
    initfunc="initmod", nout=nrow(model$PROS), parms=pars, NLVL=1)
  if (attr(out,which="istate",exact=TRUE)[1] != 2)
    stop("Integration failed.")
  dyn.unload(dllfile)
  # Return
  return(out)
}

