################################################################################
# Initializes a model, i.e. creates the shared library from user inputs.
# Actual code generation is outsourced to 'generateLib' (see below).

initModel= function(dir, xlFile, funsR, funsF, sheets) {
  # Set/check file names
  xlFile= paste(dir,xlFile,sep="/")
  if (!file.exists(xlFile))
    stop("file with model definition not found ('",xlFile,"')")
  funsR= paste(dir,funsR,sep="/")
  if (!file.exists(funsR))
    stop("file with function definitions in R not found ('",funsR,"')")
  funsF= paste(dir,funsF,sep="/")
  if (!file.exists(funsF))
    stop("file with function definitions in Fortran 95 not found ('",funsF,"')")
  # Check sheet names
  required= c("vars","pars","funs","pros","stoi")
  if ((length(sheets) != 5) || !identical(sort(names(sheets)),
    sort(required)) || !is.character(sheets))
    stop("argument 'sheets' must be a vector of strings with elements '",
      paste(required,collapse="', '"),"'")
  # Read worksheets from Excel file and export tables to a dedicated environment
  tryCatch({
    wBook= XLConnect::loadWorkbook(xlFile)
  }, error= function(e) {
    stop(paste0("failed to import workbook from file '",xlFile,"'; details: ",e))
  })
  tbl= list()
  for (i in 1:length(sheets)) {
    tryCatch({
      tmp= XLConnect::readWorksheet(object=wBook, sheet=sheets[i])
       # drop empty rows
      tmp= subset(tmp, apply(tmp, 1, function(x) {!all(is.na(x))}))
    }, error= function(e) {
      stop(paste0("failed to import data from worksheet '",sheets[i],
        "' of workbook (i.e. file) '",xlFile,"'; details: ",e))
    })
    tbl[[i]]= tmp
    names(tbl)[i]= sheets[i]
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
  command= paste0("R CMD SHLIB ",source_f_fun," ",source_f_gen," ",source_f_wrp,
    " --preclean --clean -o ",dllfile)
  if (system(command) != 0)
    stop(paste0("Error running '",command,"'"))
  invisible(file.remove(list.files(pattern=".+[.]mod")))
  return(dllfile)
}

