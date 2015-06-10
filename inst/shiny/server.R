
# Get info on parameters and variables

par= get("rodeoApp.pars",envir=globalenv())
required= c("name","label","default","user")
if (!all(required %in% names(par)))
  stop(paste0("incomplete table of parameters; the required columns are: '",
    paste(required,collapse="', '"),"'"))
par= par[,required]

var= get("rodeoApp.vars",envir=globalenv())
required= c("name","label","default")
if (!all(required %in% names(var)))
  stop(paste0("incomplete table of variables; the required columns are: '",
    paste(required,collapse="', '"),"'"))
var= var[,required]

# Initializations for plotting of reference solution
out_ref= NULL
setRefCounter= 0

################################################################################

# Define server
shinyServer(function(input, output) {

  userData= reactive({
    currentPar= data.frame(name=par$name, value=par$default, stringsAsFactors=FALSE)
    for (i in 1:nrow(currentPar)) {
      pos= match(currentPar$name[i], names(input))
      if (!is.na(pos)) {
        currentPar$value[i]= input[[names(input)[pos]]]
      }
    }
    currentVar= data.frame(name=var$name, value=var$default, mult=1, draw=FALSE,
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
      pos= match(paste0(currentVar$name[i],".draw"), names(input))
      if (!is.na(pos)) {
        currentVar$draw[i]= input[[names(input)[pos]]]
      }
    }
    return(list(
      par=setNames(as.numeric(currentPar$value), currentPar$name),
      var=setNames(as.numeric(currentVar$value), currentVar$name),
      mult=setNames(as.numeric(currentVar$mult), currentVar$name),
      draw=setNames(as.logical(currentVar$draw), currentVar$name)
    ))
  })

  output$plotStoi <- renderPlot({
    plotStoi(model=get("rodeoApp.model",envir=globalenv()),
      vars=userData()$var, pars=userData()$par,
      funsR=get("rodeoApp.funsR",envir=globalenv()))
  })

  output$plotStates <- renderPlot({
    t= seq(from=as.numeric(input$time.min), to=as.numeric(input$time.max),
      by=as.numeric(input$time.dt))
    out= simul(model=get("rodeoApp.model",envir=globalenv()),
      vars=userData()$var, pars= userData()$par,
      times=t, dllfile=get("rodeoApp.dllfile",envir=globalenv()))
    plotStates(out, out_ref, model=get("rodeoApp.model",envir=globalenv()),
      mult=userData()$mult, draw=userData()$draw,
      yrange=as.numeric(c(input$y.min,input$y.max)), logY=input$logY,
      showOld=input$showRef)
    if (input$setRef > setRefCounter) {
      out_ref <<- out
      setRefCounter <<- input$setRef 
    }
  })

})

