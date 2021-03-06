# Note: Actual code generation is outsourced to 'generateLib' (see below).

#' Create model object and generate code
#'
#' Creates a model object from user inputs and builds a shared library
#' from generated Fortran code.
#'
#' @param dir Directory where input files are located. This is assumed to apply
#'   to any input files (i.e. \code{xlFile}, \code{funsR}, \code{funsF},
#'   and possibly the values of \code{tables}).
#' @param xlFile Either the base name of file in MS Excel format holding
#'   the model definition or \code{NULL}. In the latter case, the model
#'   definition is read from delimited text files using
#'   \code{read.table(header=TRUE, sep=colsep)} where the last argument is
#'   under user control (see \code{colsep} below).
#' @param funsR Base name of file with function definitions in R.
#' @param funsF Base name of file with function definitions in Fortran 95.
#' @param tables Named vector with required elements 'vars', 'pars', 'funs',
#'   'pros', and 'stoi'. If a file name was supplied in \code{xlFile}, the
#'   values are interpreted as the names of worksheets holding the declaration
#'   of variables, parameters, functions, process rates, and stoichiometry
#'   factors, respectively. If \code{xlFile} is an empty string, the values are
#'   interpreted as the names of delimited text files holding the respective
#'   information.
#' @param colsep Specifies the column separator in text files. Only used if
#'   \code{xlFile} is \code{NULL}.
#' @param stoiAsMatrix Logical. Allows the stoichiometry matrix to be supplied
#'   as a 3-column table or as a matrix.
#' @param dllname Character string used as the name for the built library. This
#'   is the base name without file extension (must not contain path separators,
#'   white space, etc.). If set to \code{NULL} (the default), an automatic
#'   name is assigned. The library is created in the folder returned by
#'   \code{tempdir()} and it carries the system-specific extension for shared
#'   libraries according to \code{.Platform$dynlib.ext}.
#'
#' @return A list with the following components.
#' \itemize{
#'   \item{\code{model} : } Object of class \code{rodeo} representing the model.
#'   \item{\code{dllfile} : } File path of library with compiled Fortran code.
#'   \item{\code{funsR} : } Full name of the file with R functions.
#'   \item{\code{vars} : } Data frame with variable declarations.
#'   \item{\code{pars} : } Data frame with parameter declarations.
#' }
#'
#' @note Files in MS Excel format can also be created/edited with free software
#'   such as, e.g., LibreOffice Calc.
#'
#'   Spreadsheets are typically more convenient to edit as compared to
#'   delimited text files. However, with text files it is easier to track the
#'   history of a model using version control software and/or diff tools.
#'
#' @author David Kneis \email{david.kneis@@tu-dresden.de}
#'
#' @export

initModel <- function(
  dir=".", xlFile="model.xlsx", funsR="functions.r",
  funsF=list.files(path=dir, pattern=".+[.]f95$"),
  tables= c(vars="vars",pars="pars",funs="funs",pros="pros",stoi="stoi"),
  colsep=",", stoiAsMatrix=FALSE, dllname=NULL
) {
  # Set/check file names
  funsR <- paste(dir,funsR,sep="/")
  if (!all(file.exists(funsR)))
    stop("file(s) with function definitions in R not found ('",
      paste(funsR, collapse="', "),"')")
  funsF <- paste(dir,funsF,sep="/")
  if (!all(file.exists(funsF)))
    stop("file(s) with function definitions in Fortran not found ('",
      paste(funsF, collapse="', "),"')")
  # Check table names
  required <- c("vars","pars","funs","pros","stoi")
  if ((length(tables) != 5) || !identical(sort(names(tables)),
    sort(required)) || !is.character(tables))
    stop("argument 'tables' must be a character vector with elements '",
      paste(required,collapse="', '"),"'")
  # Init list to hold the model definition tables
  tbl <- list()
  # Read tables from Excel worksheets
  if (!is.null(xlFile)) {
    xlFile <- paste(dir,xlFile,sep="/")
    if (!file.exists(xlFile))
      stop(paste0("file with model definition not found ('",xlFile,"')"))
    for (i in 1:length(tables)) {
      tryCatch({
        tmp <- read_excel(path=xlFile, sheet=tables[i], col_names= TRUE,
          col_types=NULL, na="", skip=0)
         # drop empty rows
        tmp <- subset(tmp, apply(tmp, 1, function(x) {!all(is.na(x))}))
      }, error= function(e) {
        stop(paste0("failed to import data from worksheet '",tables[i],
          "' of workbook (i.e. file) '",xlFile,"'; details: ",e))
      })
      tbl[[i]] <- tmp
      names(tbl)[i] <- tables[i]
    }

  # Read tables from tabular text files
  } else {
    for (i in 1:length(tables)) {
      f <- paste(dir,tables[i],sep="/")
      if (!file.exists(f))
        stop("file with model definition, part '",names(tables)[i],"', not found ('",f,"')")
      tryCatch({
        tmp <- utils::read.table(file=f,sep=colsep,header=TRUE)
      }, error= function(e) {
        stop(paste0("failed to import data from text file '",f,
          "'; details: ",e))
      })
      tbl[[i]] <- tmp
      names(tbl)[i] <- tables[i]
    }
  }
  ##print(tbl)
  # Create rodeo object
  if (stoiAsMatrix)
    tbl[["stoi"]] <- as.matrix(tbl[["stoi"]])
  model <- rodeo$new(vars=tbl$vars, pars=tbl$pars, funs=tbl$funs,
    pros=tbl$pros, stoi=tbl$stoi,
    asMatrix=stoiAsMatrix,
    size=1)                   # GUI handles 0D models only
  # Generate shared lib
  dllfile <- generateLib(model=model, source_f_fun=funsF, dllname=dllname)
  # Return
  return(list(model=model, dllfile=dllfile, funsR=funsR,
    vars=tbl$vars, pars=tbl$pars))
}

################################################################################
# Generates shared library with deSolve-compatible derivatives code
# Inputs
#   model: rodeo object
#   source_f_fun: function definitions in Fortran
#   dllname: name of the library to be generated (basename without extension)
# Returns
#   File name of the generated shared library

generateLib <- function(model, source_f_fun, dllname=NULL) {
  # Get name of temporary folder
  tmpdir <- gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE)
  # Generate code to compute derivatives
  code <- model$generate(name="derivs",lang="f95")
  source_f_gen <- gsub(pattern="\\", replacement="/",
    x=paste0(tempfile(), ".f95"), fixed=TRUE)
  write(x=code, file=source_f_gen)
#  cat("code written to",source_f_gen)
  # Create wrapper code for compatibility with deSolve; single spatial level
  code <- solverInterface(1, "derivs")
  source_f_wrp <- gsub(pattern="\\", replacement="/",
    x=paste0(tempfile(), ".f95"), fixed=TRUE)
  write(x=code, file=source_f_wrp)
#  cat("code written to",source_f_wrp)
  # Compile code (in temporary folder to avoid permission problems)
  if (is.null(dllname))
    dllname= basename(tempfile())
  dllfile <- paste0(tmpdir,"/",dllname,.Platform$dynlib.ext)
  if (file.exists(dllfile))
    invisible(file.remove(dllfile))
  file.copy(from=source_f_fun, to=tmpdir, overwrite=TRUE)
  source_f_fun= paste0(tmpdir,"/",basename(source_f_fun))
  wd <- getwd()
  setwd(tmpdir)
  command <- paste("R CMD SHLIB",paste(shQuote(source_f_fun), collapse=" "),
    shQuote(source_f_gen),shQuote(source_f_wrp),
    "--preclean --clean -o",shQuote(dllfile))
  if (system(command) != 0)
    stop(paste0("Error running '",command,"'"))
  invisible(file.remove(list.files(pattern=".+[.]mod")))
  setwd(wd)
  return(dllfile)
}

