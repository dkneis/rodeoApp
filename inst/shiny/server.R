
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
sim_ref= NULL

buttonCount= c(
  setRef= 0,
  saveSettings= 0,
  saveImage= 0,
  tShiftLeft= 0,
  tShiftRight= 0,
  tZoomIn= 0,
  tZoomOut= 0,
  tReset= 0
)

taxis= c(
  center= NA,
  width= NA
)

################################################################################

# Define server
shinyServer(function(input, output) {

  # Set parameters and variables (--> items depend on the loaded model)
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

  # Carry out a simulation run
  sim= reactive({
    inp= userData()
    # Steady state simulation
    names_steady= var$name[which(as.logical(var$steady))]
    if (length(names_steady) > 0) {
      sim= stst(model=get("rodeoApp.model",envir=globalenv()),
        vars=inp$var, pars= inp$par,
        dllfile=get("rodeoApp.dllfile",envir=globalenv()),
        rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
      inp$var[names_steady]= sim$y[names_steady]
    }
    # Dynamic simulation
    t= seq(from=as.numeric(input$.time.start), to=as.numeric(input$.time.end),
      by=as.numeric(input$.time.dt))
    sim= simul(model=get("rodeoApp.model",envir=globalenv()),
      vars=inp$var, pars= inp$par,
      times=t, dllfile=get("rodeoApp.dllfile",envir=globalenv()),
      rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
    # Save as reference
    if (input$setRef > buttonCount["setRef"]) {
      sim_ref <<- sim
      buttonCount["setRef"] <<- input$setRef 
    }
    return(sim)
  })

  # Plot stoichiometry matrix
  output$visStoi <- renderText({
    inp= userData()
    visStoi(model=get("rodeoApp.model",envir=globalenv()),
      vars=inp$var, pars=inp$par,
      funsR=get("rodeoApp.funsR",envir=globalenv()))
  })


  # Plot state variables
  output$plotStates <- renderPlot({
    # Observe state of interactive time scrolling/zooming
    if (input$tReset > buttonCount["tReset"]) {
      buttonCount["tReset"] <<- input$tReset
      reset= TRUE
    } else {
      reset= FALSE
    }
    if ((sum(c(buttonCount["tShiftLeft"], buttonCount["tShiftRight"])) == 0) || reset)
      if (grepl(pattern="[0-9]",x=input$.taxis.center)) {
        taxis["center"] <<- as.numeric(input$.taxis.center)
      } else {
        taxis["center"] <<- as.numeric(input$.time.start) + 0.5 *
          (as.numeric(input$.time.end) - as.numeric(input$.time.start))
      }
    if ((sum(c(buttonCount["tZoomIn"], buttonCount["tZoomOut"])) == 0) || reset)
      if (grepl(pattern="[0-9]",x=input$.taxis.width)) {
        taxis["width"] <<- as.numeric(input$.taxis.width)
      } else {
        taxis["width"] <<- as.numeric(input$.time.end) - as.numeric(input$.time.start)
      }
    if (input$tZoomIn > buttonCount["tZoomIn"]) {
      buttonCount["tZoomIn"] <<- input$tZoomIn
      taxis["width"] <<- taxis["width"] * 2/3
    }
    if (input$tZoomOut > buttonCount["tZoomOut"]) {
      buttonCount["tZoomOut"] <<- input$tZoomOut
      taxis["width"] <<- taxis["width"] * 3/2
    }
    if (input$tShiftLeft > buttonCount["tShiftLeft"]) {
      buttonCount["tShiftLeft"] <<- input$tShiftLeft
      taxis["center"] <<- taxis["center"] - 0.2 * taxis["width"]
    }
    if (input$tShiftRight > buttonCount["tShiftRight"]) {
      buttonCount["tShiftRight"] <<- input$tShiftRight
      taxis["center"] <<- taxis["center"] + 0.2 * taxis["width"]
    }

    # Graphics
    plt= function() {
      tmin= taxis["center"] - 0.5 * taxis["width"]
      tmax= taxis["center"] + 0.5 * taxis["width"]

      plotStates(sim(), sim_ref, input$.time.unit, input$.time.base,
        model=get("rodeoApp.model",envir=globalenv()),
        mult=userData()$mult, show=userData()$show,
        rangeT=c(tmin,tmax),
        rangeY=as.numeric(c(input$.yaxis.min,input$.yaxis.max)),
        gridT=input$.taxis.grid,
        gridY=input$.yaxis.grid,
        logY=input$.yaxis.log,
        labelY=input$.yaxis.label,
        showOld=input$showRef,
        obs=get("rodeoApp.obs",envir=globalenv()))
    }
    # plot to screen
    plt()
    # plot to file
    if (input$saveImage > buttonCount["saveImage"]) {
      buttonCount["saveImage"] <<- input$saveImage
      plotPNG(func=plt, filename=paste0(input$.png.dir,"/",
        format(Sys.time(),"%Y-%m-%dT%H%M%S"),".png"),
        width=as.numeric(input$.png.width), height=as.numeric(input$.png.height),
        res=as.numeric(input$.png.res))
    }
  })

  # Save settings on request
  observe({
    if (input$saveSettings > buttonCount["saveSettings"]) {
      buttonCount["saveSettings"] <<- input$saveSettings
      nam= names(input)[grepl(pattern="^[.][a-zA-Z._]+", x=names(input))]
      sets= vector("character", length(nam))
      for (i in 1:length(nam)) {  # because single brackets, e.g. input[nam], not allowed
        sets[i]= paste0("'",input[[nam[i]]],"'")
        names(sets)[i]= nam[i]
      }
      write(x=paste(names(sets),sets,sep="=",collapse="\n"),
        file=get("rodeoApp.fileSettings",envir=globalenv()))
    }
  })

})

