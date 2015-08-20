library("rodeoApp")

# Load data created by runGUI
dirs= c(".",gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE))
loaded= FALSE
for (d in dirs) {
  if (file.exists(paste0(d,"/rodeoAppData.rda"))) {
    load(file=paste0(d,"/rodeoAppData.rda"))
    loaded= TRUE
    break
  }
}
if (!loaded)
  stop(paste0("startup data not found in folder(s): '",
    paste(dirs,collapse="', '"),"'"))

# Set working directory
setwd(rodeoAppData$wd)

# Get info on parameters and variables

par= rodeoAppData$pars
required= c("name","label","default","user", "html")
if (!all(required %in% names(par)))
  stop(paste0("incomplete table of parameters; the required columns are: '",
    paste(required,collapse="', '"),"'"))
par= par[,required]

var= rodeoAppData$vars
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
  tReset= 0,
  yZoomIn= 0,
  yZoomOut= 0,
  yReset= 0
)

taxis= c(center= NA, width= NA)
yaxis= c(min= NA, max= NA)

################################################################################

# Define server
shinyServer(function(input, output) {

  # Get parameters and variables (--> items depend on the loaded model)
  userPars= reactive({
    currentPar= data.frame(name=par$name, value=par$default, stringsAsFactors=FALSE)
    for (i in 1:nrow(currentPar)) {
      pos= match(currentPar$name[i], names(input))
      if (!is.na(pos)) {
        currentPar$value[i]= input[[names(input)[pos]]]
      }
    }
    return(setNames(as.numeric(currentPar$value), currentPar$name))
  })
  userVars= reactive({
    currentVar= data.frame(name=var$name, value=var$default, stringsAsFactors=FALSE)
    for (i in 1:nrow(currentVar)) {
      pos= match(currentVar$name[i], names(input))
      if (!is.na(pos)) {
        currentVar$value[i]= input[[names(input)[pos]]]
      }
    }
    return(setNames(as.numeric(currentVar$value), currentVar$name))
  })

  # Get options for plotting of variables (--> items depend on the loaded model)
  showVars= reactive({
    currentVar= data.frame(name=var$name, show=as.logical(var$show), stringsAsFactors=FALSE)
    for (i in 1:nrow(currentVar)) {
      pos= match(paste0(currentVar$name[i],".show"), names(input))
      if (!is.na(pos)) {
        currentVar$show[i]= input[[names(input)[pos]]]
      }
    }
    return(setNames(as.logical(currentVar$show), currentVar$name))
  })
  multVars= reactive({
    currentVar= data.frame(name=var$name, mult=var$mult, stringsAsFactors=FALSE)
    for (i in 1:nrow(currentVar)) {
      pos= match(paste0(currentVar$name[i],".mult"), names(input))
      if (!is.na(pos)) {
        currentVar$mult[i]= input[[names(input)[pos]]]
      }
    }
    return(setNames(as.numeric(currentVar$mult), currentVar$name))
  })

  # Carry out a simulation run
  sim= reactive({
    inp= list(var=userVars(), par=userPars())
    # Steady state simulation
    names_steady= var$name[which(as.logical(var$steady))]
    if (length(names_steady) > 0) {
      sim= stst(model=rodeoAppData$model,
        vars=inp$var, pars= inp$par,
        dllfile=rodeoAppData$dllfile,
        rtol=setNames(var$rtol, var$name), atol=setNames(var$atol, var$name))
      inp$var[names_steady]= sim$y[names_steady]
    }
    # Dynamic simulation
    t= seq(from=as.numeric(input$.time.start), to=as.numeric(input$.time.end),
      by=as.numeric(input$.time.dt))
    sim= simul(model=rodeoAppData$model,
      vars=inp$var, pars= inp$par,
      times=t, dllfile=rodeoAppData$dllfile,
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
    visStoi(model=rodeoAppData$model,
      vars=userVars(), pars=userPars(),
      funsR=rodeoAppData$funsR)
  })

  # Update axis definitions
  # Note: We need to manipulate global values to make zooming etc. reversible
  updateAxDefs= reactive({
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
    # Observe state of interactive y-zooming
    if (input$yReset > buttonCount["yReset"]) {
      buttonCount["yReset"] <<- input$yReset
      reset= TRUE
    } else {
      reset= FALSE
    }
    if ((sum(c(buttonCount["yZoomIn"], buttonCount["yZoomOut"])) == 0) || reset) {
      yaxis["min"] <<- as.numeric(input$.yaxis.min)
      yaxis["max"] <<- as.numeric(input$.yaxis.max)
    }
    if (input$yZoomIn > buttonCount["yZoomIn"]) {
      buttonCount["yZoomIn"] <<- input$yZoomIn
      yaxis["max"] <<- yaxis["max"] - 0.2 * (yaxis["max"] - yaxis["min"])
    }
    if (input$yZoomOut > buttonCount["yZoomOut"]) {
      buttonCount["yZoomOut"] <<- input$yZoomOut
      yaxis["max"] <<- yaxis["max"] + 0.2 * (yaxis["max"] - yaxis["min"])
    }
    return(list(
      taxis=c(min=taxis[["center"]] - 0.5 * taxis[["width"]],
              max=taxis[["center"]] + 0.5 * taxis[["width"]]),
      yaxis=c(min=yaxis[["min"]], max=yaxis[["max"]])
    ))
  })


  # Plot state variables
  output$plotStates <- renderPlot({
    axDefs= updateAxDefs()
    plotStates(sim(), sim_ref, input$.time.unit, input$.time.base,
      model=rodeoAppData$model,
      mult=multVars(), show=showVars(),
      rangeT=c(axDefs$taxis["min"], axDefs$taxis["max"]),
      rangeY=c(axDefs$yaxis["min"], axDefs$yaxis["max"]),
      gridT=input$.taxis.grid, gridY=input$.yaxis.grid, logY=input$.yaxis.log,
      labelY=input$.yaxis.label, showOld=input$showRef, obs=rodeoAppData$obs
    )
  })

  # Download handler
  output$saveImage <- downloadHandler(
    # Function returning a file name
    filename = function() {
		  paste0("rodeoAppImage_",format(Sys.time(),"%Y-%m-%dT%H%M%S"),".png")
	  },
    # Function writing data to its argument 'file'
    content = function(file) {
      tmpFun= function() {
        axDefs= updateAxDefs()
        plotStates(sim(), sim_ref, input$.time.unit, input$.time.base,
          model=rodeoAppData$model,
          mult=multVars(), show=showVars(),
          rangeT=c(axDefs$taxis["min"], axDefs$taxis["max"]),
          rangeY=c(axDefs$yaxis["min"], axDefs$yaxis["max"]),
          gridT=input$.taxis.grid, gridY=input$.yaxis.grid, logY=input$.yaxis.log,
          labelY=input$.yaxis.label, showOld=input$showRef, obs=rodeoAppData$obs
        )
      }
      plotPNG(func=tmpFun, filename=file, width=as.numeric(input$.png.width),
        height=as.numeric(input$.png.height), res=as.numeric(input$.png.res))
    }
  )

  # Save settings on request
  if (!rodeoAppData$serverMode) {
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
          file=rodeoAppData$fileSettings)
      }
    })
  }

})

