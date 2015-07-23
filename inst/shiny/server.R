
# Get info on parameters and variables

par= get("rodeoApp.pars",envir=globalenv())
required= c("name","label","default","user", "html")
if (!all(required %in% names(par)))
  stop(paste0("incomplete table of parameters; the required columns are: '",
    paste(required,collapse="', '"),"'"))
par= par[,required]

var= get("rodeoApp.vars",envir=globalenv())
required= c("name","label","default", "html", "tex", "mult", "show", "rtol", "atol", "steady")
if (!all(required %in% names(var)))
  stop(paste0("incomplete table of variables; the required columns are: '",
    paste(required,collapse="', '"),"'"))
var= var[,required]

# Initializations for plotting of reference solution
out_ref= NULL
setRefCounter= 0

saveSettingsCounter= 0

################################################################################

# Define server
shinyServer(function(input, output) {

  # Set parameters and variables
  userData= reactive({
    currentPar= data.frame(name=par$name, value=par$default, stringsAsFactors=FALSE)
    for (i in 1:nrow(currentPar)) {
      pos= match(currentPar$name[i], names(input))
      if (!is.na(pos)) {
        currentPar$value[i]= input[[names(input)[pos]]]
      }
    }
    currentVar= data.frame(name=var$name, value=var$default,
      mult=var$mult,
      show=as.logical(var$show),
      stringsAsFactors=FALSE)
    for (i in 1:nrow(currentVar)) {
      pos= match(currentVar$name[i], names(input))
      if (!is.na(pos)) {
        currentVar$value[i]= input[[names(input)[pos]]]
      }
      pos= match(paste0(currentVar$name[i],".mult"), names(input))
      if (!is.na(pos)) {
        currentVar$mult[i]= input[[names(input)[pos]]]
      }
      pos= match(paste0(currentVar$name[i],".show"), names(input))
      if (!is.na(pos)) {
        currentVar$show[i]= input[[names(input)[pos]]]
      }
    }
    return(list(
      par=setNames(as.numeric(currentPar$value), currentPar$name),
      var=setNames(as.numeric(currentVar$value), currentVar$name),
      mult=setNames(as.numeric(currentVar$mult), currentVar$name),
      show=setNames(as.logical(currentVar$show), currentVar$name)
    ))
  })

  # Plot stoichiometry matrix
  output$visStoi <- renderText({
  visStoi(model=get("rodeoApp.model",envir=globalenv()),
      vars=userData()$var, pars=userData()$par,
      funsR=get("rodeoApp.funsR",envir=globalenv()))
  })

  # Simulate and plot state variables
  output$plotStates <- renderPlot({

    # Store user-supplied initial values for possible further modification
    v= userData()$var

    # Steady state simulation
    names_steady= var$name[which(as.logical(var$steady))]
    if (length(names_steady) > 0) {
      out= stst(model=get("rodeoApp.model",envir=globalenv()),
        vars=v, pars= userData()$par,
        dllfile=get("rodeoApp.dllfile",envir=globalenv()),
        rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
      v[names_steady]= out$y[names_steady]
    }

    # Dynamic simulation
    t= seq(from=as.numeric(input$.time.start), to=as.numeric(input$.time.end),
      by=as.numeric(input$.time.dt))
    out= simul(model=get("rodeoApp.model",envir=globalenv()),
      vars=v, pars= userData()$par,
      times=t, dllfile=get("rodeoApp.dllfile",envir=globalenv()),
      rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
    # Graphics
    plotStates(out, out_ref, model=get("rodeoApp.model",envir=globalenv()),
      mult=userData()$mult, show=userData()$show,
      rangeT=as.numeric(c(input$.taxis.min,input$.taxis.max)),
      rangeY=as.numeric(c(input$.yaxis.min,input$.yaxis.max)),
      gridT=input$.taxis.grid,
      gridY=input$.yaxis.grid,
      logY=input$.yaxis.log,
      showOld=input$showRef,
      obs=get("rodeoApp.obs",envir=globalenv()))
    if (input$setRef > setRefCounter) {
      out_ref <<- out
      setRefCounter <<- input$setRef 
    }
  })

  # Save settings on request
  observe({
    if (input$saveSettings > saveSettingsCounter) {
      nam= names(input)[grepl(pattern="^[.][a-zA-Z._]+", x=names(input))]
      sets= vector("character", length(nam))
      for (i in 1:length(nam)) {  # because single brackets, e.g. input[nam], not allowed
        sets[i]= input[[nam[i]]]
        names(sets)[i]= nam[i]
      }
      write(x=paste(names(sets),sets,sep="=",collapse="\n"),
        file=get("rodeoApp.fileSettings",envir=globalenv()))
      saveSettingsCounter <<- input$saveSettings
    }
  })

})

