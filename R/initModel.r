# Note: Actual code generation is outsourced to 'generateLib' (see below).

#' Create model object and generate code
#'
#' Creates a model object from user inputs and builds a shared library
#' from generated Fortran code.
#'
#' @param dir Directory where input files are located. This value is added as a
#'   prefix to any file names (i.e. \code{xlFile}, \code{funsR}, \code{funsF},
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
#'
#' @return A list with the following components.
#' \itemize{
#'   \item{\code{model} : } Object of class \code{rodeo} representing the model.
#'   \item{\code{dllfile} : } Shared library with compiled Fortran code.
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

initModel= function(
  dir="", xlFile="model.xlsx", funsR="functions.r", funsF="functions.f95",
  tables= c(vars="vars",pars="pars",funs="funs",pros="pros",stoi="stoi"),
  colsep=";"
) {
  # Set/check file names
  funsR= paste(dir,funsR,sep="/")
  if (!file.exists(funsR))
    stop("file with function definitions in R not found ('",funsR,"')")
  funsF= paste(dir,funsF,sep="/")
  if (!file.exists(funsF))
    stop("file with function definitions in Fortran 95 not found ('",funsF,"')")
  # Check table names
  required= c("vars","pars","funs","pros","stoi")
  if ((length(tables) != 5) || !identical(sort(names(tables)),
    sort(required)) || !is.character(tables))
    stop("argument 'tables' must be a character vector with elements '",
      paste(required,collapse="', '"),"'")
  # Init list to hold the model definition tables
  tbl= list()
  # Read tables from Excel worksheets
  if (!is.null(xlFile)) {
    xlFile= paste(dir,xlFile,sep="/")
    if (!file.exists(xlFile))
      stop("file with model definition not found ('",xlFile,"')")
    tryCatch({
      wBook= XLConnect::loadWorkbook(xlFile)
    }, error= function(e) {
      stop(paste0("failed to import workbook from file '",xlFile,"'; details: ",e))
    })
    for (i in 1:length(tables)) {
      tryCatch({
        tmp= XLConnect::readWorksheet(object=wBook, sheet=tables[i])
         # drop empty rows
        tmp= subset(tmp, apply(tmp, 1, function(x) {!all(is.na(x))}))
      }, error= function(e) {
        stop(paste0("failed to import data from worksheet '",tables[i],
          "' of workbook (i.e. file) '",xlFile,"'; details: ",e))
      })
      tbl[[i]]= tmp
      names(tbl)[i]= tables[i]
    }
  # Read tables from tabular text files
  } else {
    for (i in 1:length(tables)) {
      f= paste(dir,tables[i],sep="/")
      if (!file.exists(f))
        stop("file with model definition, part '",names(tables)[i],"', not found ('",f,"')")
      tryCatch({
        tmp= read.table(file=f,sep=colsep,header=TRUE)
      }, error= function(e) {
        stop(paste0("failed to import data from text file '",f,
          "'; details: ",e))
      })
      tbl[[i]]= tmp
      names(tbl)[i]= tables[i]
    }
  }
  ##print(tbl)
  # Create rodeo object
  model= new("rodeo", vars=tbl$vars, pars=tbl$pars, funs=tbl$funs,
    pros=tbl$pros, stoi=tbl$stoi)
  # Generate shared lib
  dllfile= generateLib(model=model, source_f_fun=funsF)
  # Return
  return(list(model=model, dllfile=dllfile, funsR=funsR,
    vars=tbl$vars, pars=tbl$pars))
}

################################################################################
# Generates shared library with deSolve-compatible derivatives code
# Inputs
#   model: rodeo object
#   source_f_fun: function definitions in Fortran
# Returns
#   File name of the generated shared library

generateLib= function(model, source_f_fun) {
  # Generate code to compute derivatives
  code= model$generate(name="derivs",lang="f95")
  source_f_gen= gsub(pattern="\\", replacement="/",
    x=paste0(tempfile(), ".f95"), fixed=TRUE)
  write(x=code, file=source_f_gen)
#  cat("code written to",source_f_gen)
  # Create wrapper code for compatibility with deSolve
  code= paste(
    "! Definition of the number of spatial levels",
    "module spatial_dimension",
    "implicit none",
    "integer, parameter:: NLVL=1",
    "end module",
    " ",
    "! Generic routine for parameter initialization",
    "subroutine initmod(extfun)",
    " use dimensions_and_indices   ! Module is provided by the generated code",
    " use spatial_dimension",
    " external extfun",
    " double precision, dimension(NPAR*NLVL):: par",
    " common /params/ par",
    " call extfun(NPAR*NLVL, par)",
    "end subroutine",
    " ",
    "! Generic wrapper around the generated code",
    "subroutine derivs_wrapped (neq, t, y, ydot, yout, ip)",
    " use dimensions_and_indices   ! Module is provided by the generated code",
    " use spatial_dimension",
    " implicit none",
    " ! Inputs",
    " integer, intent(in):: neq",
    " double precision, intent(in):: t",
    " double precision, dimension(neq), intent(in):: y",
    " integer, dimension(*), intent(in)::ip",
    " ! Outputs",
    " double precision, dimension(neq), intent(out)::ydot",
    " double precision, dimension(ip(2)), intent(out)::yout",
    " ! Import parameters",
    " double precision, dimension(NPAR*NLVL):: par",
    " common /params/ par",
    " !Call to generated code",
    " call derivs(t, y, par, NLVL, ydot, yout)",
    "end subroutine",
    sep="\n")
  source_f_wrp= gsub(pattern="\\", replacement="/",
    x=paste0(tempfile(), ".f95"), fixed=TRUE)
  write(x=code, file=source_f_wrp)
  # Compile code
  dllname= "mySharedLib"
  dllfile= paste0(gsub(pattern="\\", replacement="/",
    x=tempdir(), fixed=TRUE),"/",dllname,.Platform$dynlib.ext)
  if (file.exists(dllfile))
    invisible(file.remove(dllfile))
  command= paste0("R CMD SHLIB ",shQuote(source_f_fun)," ",shQuote(source_f_gen),
    " ",shQuote(source_f_wrp),
    " --preclean --clean -o ",shQuote(dllfile))
  if (system(command) != 0)
    stop(paste0("Error running '",command,"'"))
  invisible(file.remove(list.files(pattern=".+[.]mod")))
  return(dllfile)
}

