
# Get info on parameters and variables

par= get("rodeoApp.pars",envir=globalenv())
required= c("name","label","default","user", "html")
if (!all(required %in% names(par)))
  stop(paste0("incomplete table of parameters; the required columns are: '",
    paste(required,collapse="', '"),"'"))
par= par[,required]

var= get("rodeoApp.vars",envir=globalenv())
required= c("name","label","default", "html", "mult", "show", "rtol", "atol")
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

  output$visStoi <- renderText({
  visStoi(model=get("rodeoApp.model",envir=globalenv()),
      vars=userData()$var, pars=userData()$par,
      funsR=get("rodeoApp.funsR",envir=globalenv()))
  })

  output$plotStates <- renderPlot({
    t= seq(from=as.numeric(input$time.min), to=as.numeric(input$time.max),
      by=as.numeric(input$time.dt))
    out= simul(model=get("rodeoApp.model",envir=globalenv()),
      vars=userData()$var, pars= userData()$par,
      times=t, dllfile=get("rodeoApp.dllfile",envir=globalenv()),
      rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
    plotStates(out, out_ref, model=get("rodeoApp.model",envir=globalenv()),
      mult=userData()$mult, show=userData()$show,
      yrange=as.numeric(c(input$y.min,input$y.max)), logY=input$logY,
      showOld=input$showRef)
    if (input$setRef > setRefCounter) {
      out_ref <<- out
      setRefCounter <<- input$setRef 
    }
  })

})

