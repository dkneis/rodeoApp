#' Run rodeo-based model in shiny GUI
#'
#' Runs a rodeo-based model in a shiny GUI to allow for variation of parameters
#' and/or initial values.
#'
#' @inheritParams initModel
#' @param obs If not \code{NULL}, this must be a data frame with observed values
#'   of the state variables. Time information (numeric) is expected in the first
#'   column. Columns with observation data (if existing) must be named like the
#'   simulated state variables. Missing values must be marked as \code{NA}.
#' @param serverMode Defaults to \code{FALSE}. If set to \code{TRUE}, data
#'   required by the GUI are saved to disk but the shiny app is
#'   \emph{not} run.
#'
#' @return If \code{serverMode} is \code{FALSE}, the function returns
#'   \code{NULL}. Otherwise it returns a vector holding the names of 3
#'   files needed to run the GUI.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export
#'
#' @examples
#' \dontrun{
#'   library(rodeoApp)
#'   runGUI(dir=system.file("examples/DRT", package="rodeoApp"))
#' }

runGUI <- function(
  dir=".", xlFile="model.xlsx", funsR="functions.r",
  funsF=list.files(path=dir, pattern=".+[.]f95$"),
  tables= c(vars="vars",pars="pars",funs="funs",pros="pros",stoi="stoi"),
  colsep=",", stoiAsMatrix=FALSE, obs=NULL, dllname=NULL, serverMode=FALSE
) {
  dir <- normalizePath(dir, winslash="/")

  # Run init
  ini <- initModel(dir=dir, xlFile=xlFile, funsR=funsR, funsF=funsF,
    tables=tables, colsep=colsep, stoiAsMatrix=stoiAsMatrix, dllname=dllname)

  if (serverMode) {
    dirSettings <- "."  # button to save files not available anyway
  } else {
    if (as.integer(file.access(dir, mode = 2)) == 0) {
      dirSettings <- dir           # if model folder is writeable
    } else {
      dirSettings <- getwd()       # otherwise
    }
  }

  # Save data to file (to be loaded in server/ui)
  rodeoAppData <- list(
    model= ini$model,
    dllfile= if (serverMode) basename(ini$dllfile) else ini$dllfile,
    funsR= if (serverMode) basename(ini$funsR) else ini$funsR,
    vars= ini$vars,
    pars= ini$pars,
    obs= obs,
    wd= if (serverMode) "." else getwd(),
    fileSettings= paste0(dirSettings,"/rodeoApp.savedSettings"),
    serverMode=serverMode
  )
  # NOTE: File/path name must be consistent with server/ui
  rodeoAppDataFile <- paste0(gsub(pattern="\\", replacement="/", x=tempdir(),
    fixed=TRUE), "/rodeoAppData.rda")
  save(rodeoAppData, file=rodeoAppDataFile, ascii=TRUE)
  rm(rodeoAppData)

  # Start shiny app
  if (serverMode) {
    return(c(rodeoAppDataFile, ini$dllfile, ini$funsR))
  } else {
    shiny::runApp(system.file("shiny", package="rodeoApp"))
    return(invisible(NULL))
  }
}

