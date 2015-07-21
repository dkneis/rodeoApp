################################################################################
# Method to generate GUI code

ui_generate= function(vars, pars) {

  labStyle= "font-family: Arial Narrow; line-height: 10px; color: #3E566A"
  headStyle= "font-weight: bold; color: #3E566A"
  newline="\n"

  # Settings
  if (file.exists(get("rodeoApp.fileSettings",envir=globalenv()))) {
    tempenv= new.env()
    sys.source(file=get("rodeoApp.fileSettings",envir=globalenv()), envir=tempenv)
    sett= list(
      time.start= get("time.start",tempenv),
      time.end=   get("time.end",tempenv),
      time.dt=    get("time.dt",tempenv),
      taxis.min=  get("taxis.min",tempenv),
      taxis.max=  get("taxis.max",tempenv),
      yaxis.min=  get("yaxis.min",tempenv),
      yaxis.max=  get("yaxis.max",tempenv),
      yaxis.log=  get("yaxis.log",tempenv)
    )
    rm(tempenv)
  } else {
    sett= list(
      time.start=0,
      time.end=10,
      time.dt=1,
      taxis.min=0,
      taxis.max=10,
      yaxis.min= min(vars$default)*ifelse(min(vars$default) < 0,2,0.5),
      yaxis.max= max(vars$default)*ifelse(max(vars$default) < 0,0.5,2),
      yaxis.log=FALSE
    )
  }

  # Dynamic code: Text boxes for parameters
  inputPars= function() {
    code=""
    rows= which(as.logical(pars$user))
    if (length(rows) > 0) {
      for (i in rows) {
        code=paste0(code,"      div(style='",labStyle,"',textInput('",pars$name[i],"', label = '",
          pars$label[i],"', value=",pars$default[i],"))",
          ifelse(i==rows[length(rows)],"",","),newline)
      }
    } else {
      code=paste0(code,"p('No visible items')",newline)
    }
    return(code)
  }

  # Dynamic code: Text boxes for variables
  inputVars= function() {
    code=""
    for (i in 1:nrow(vars)) {
      code=paste0(code,"        fluidRow(",newline)
      code=paste0(code,"          column(5,",newline)
      code=paste0(code,"            div(style='",labStyle,"',textInput('",vars$name[i],
        "', label = '",vars$label[i],"', value=",vars$default[i],"))",newline)
      code=paste0(code,"          ),",newline)
      code=paste0(code,"          column(4,",newline)
      code=paste0(code,"            div(style='",labStyle,"',textInput('",vars$name[i],
        ".mult', label = ' ', value=",vars$mult[i],"))",newline)
      code=paste0(code,"          ),",newline)
      code=paste0(code,"          column(3,",newline)
      code=paste0(code,"            div(style='",labStyle,"',checkboxInput('",vars$name[i],
        ".show', label = ' ', value=",as.logical(vars$show[i]),"))",newline)
      code=paste0(code,"          )",newline)
      code=paste0(code,"        )",ifelse(i==nrow(vars),"",","),newline)
    }
    return(code)
  }

  # --- BEGIN OF UI CODE ---
  code= paste0(
  "
  # THIS IS A GENERATED FILE -- DO NOT EDIT
  shinyUI(
    navbarPage('rodeoApp',

      # New tab panel
      tabPanel('Dynamic simulation',
        fluidRow(
          column(2,
            p(style='",headStyle,"', 'Parameters')
          ),
          column(2,
            p(style='",headStyle,"', 'Variables'),
            fluidRow(
              column(5,
                p(style='",headStyle,"', 'Init.')
              ),
              column(4,
                p(style='",headStyle,"', 'Mult.')
              ),
              column(3,
                p(style='",headStyle,"', 'On')
              )
            )
          ),
          column(2, p('')),
          column(2, div(style='",labStyle,"',actionButton('setRef', label='Set as reference'))),
          column(2, div(style='",labStyle,"',checkboxInput('showRef', label='Show reference', value = FALSE)))
        ),
        fluidRow(
          column(2,style = 'overflow-y:scroll; max-height: 800px',
    ",
    # Parameters
    inputPars(),
    "
          ),
          column(2,style = 'overflow-y:scroll; max-height: 800px',
    ",
    # Variables
    inputVars(),
    "
          ),
          column(8,
            fluidRow(
              column(12,
                plotOutput(outputId='plotStates', height='600px', width='90%')
              )
            )
          )
        )
      ), # End tabPanel

      # New tab panel
      tabPanel('Peterson matrix',
        fluidRow(
          column(12,
            htmlOutput(outputId='visStoi', inline=FALSE)
          )
        )
      ), # End tabPanel

      # New tab panel
      tabPanel('General settings',
        fluidRow(
          column(2, div(style='",labStyle,"',actionButton('saveSettings', label='Save settings'))),
          column(10, p('",paste0('Target file: ',get('rodeoApp.fileSettings',envir=globalenv())),"'))
        ),
        tags$hr(),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Simulation time')),
          column(1, div(style='",labStyle,"',textInput('time.start', label = 'start', value=",sett$time.start,"))),
          column(1, div(style='",labStyle,"',textInput('time.end', label = 'end', value=",sett$time.end,"))),
          column(1, div(style='",labStyle,"',textInput('time.dt', label = 'step', value=",sett$time.dt,")))
        ),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Time axis limits')),
          column(1, div(style='",labStyle,"',textInput('taxis.min', label = 'min.', value=",sett$taxis.min,"))),
          column(1, div(style='",labStyle,"',textInput('taxis.max', label = 'max.', value=",sett$taxis.max,")))
        ),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Y axis limits')),
          column(1, div(style='",labStyle,"',textInput('yaxis.min', label = 'min.', value=",sett$yaxis.min,"))),
          column(1, div(style='",labStyle,"',textInput('yaxis.max', label = 'max.', value=",sett$yaxis.max,"))),
          column(1, div(style='",labStyle,"',checkboxInput('yaxis.log', label='Log scale', value=",sett$yaxis.log,")))
        )
      ) # End tabPanel

    ) # End Navbar page
  ) # End shinyUI
  "
  ) 
  # --- END OF UI CODE ---
  return(code)
}



#code= ui_generate(vars=get("rodeoApp.vars",envir=globalenv()),
#  pars=get("rodeoApp.pars",envir=globalenv()))
#f= tempfile()
#write(x=code, file=gsub(pattern="\\",replacement="/",x=f,fixed=TRUE))
#cat("written to",f,"\n")

################################################################################
# Create and execute GUI code

eval(parse(text=ui_generate(
  vars=get("rodeoApp.vars",envir=globalenv()),
  pars=get("rodeoApp.pars",envir=globalenv())
)))

