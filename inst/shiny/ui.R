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
      .time.start= get(".time.start",tempenv),
      .time.end=   get(".time.end",tempenv),
      .time.dt=    get(".time.dt",tempenv),
      .time.base=  get(".time.base",tempenv),
      .time.unit=  get(".time.unit",tempenv),
      .taxis.min=  get(".taxis.min",tempenv),
      .taxis.max=  get(".taxis.max",tempenv),
      .taxis.grid= get(".taxis.grid",tempenv),
      .yaxis.min=  get(".yaxis.min",tempenv),
      .yaxis.max=  get(".yaxis.max",tempenv),
      .yaxis.grid= get(".yaxis.grid",tempenv),
      .yaxis.log=  get(".yaxis.log",tempenv),
      .png.width=  get(".png.width",tempenv),
      .png.height= get(".png.height",tempenv),
      .png.res=    get(".png.res",tempenv),
      .png.file=   get(".png.file",tempenv)
    )
    rm(tempenv)
  } else {
    sett= list(
      .time.start=0,
      .time.end=10,
      .time.dt=1,
      .time.base="1970-01-01T00:00:00",
      .time.unit="seconds",
      .taxis.min=0,
      .taxis.max=10,
      .taxis.grid=FALSE,
      .yaxis.min= min(vars$default)*ifelse(min(vars$default) < 0,2,0.5),
      .yaxis.max= max(vars$default)*ifelse(max(vars$default) < 0,0.5,2),
      .yaxis.grid=FALSE,
      .yaxis.log=FALSE,
      .png.width=1200,
      .png.height=800,
      .png.res=150,
      .png.file=gsub(pattern="\\", replacement="/", x=tempfile(fileext=".png"), fixed=TRUE)
    )
  }

  # Dynamic code: Text boxes for parameters
  inputPars= function() {
    code=""
    rows= which(as.logical(pars$user))
    if (length(rows) > 0) {
      for (i in 1:length(rows)) {
        # Begin param name
        code=paste0(code,"        fluidRow(",newline)
        code=paste0(code,"          column(12,",newline)
        code=paste0(code,"            div(style='",labStyle,"', '",pars$label[rows[i]],"')",newline)
        code=paste0(code,"          )",newline)
        code=paste0(code,"        ),",newline)
        # End param name
        code=paste0(code,"        fluidRow(",newline)
        code=paste0(code,"          column(12,",newline)
        code=paste0(code,"            div(style='",labStyle,
          "',textInput('",pars$name[rows[i]],
          "', label = '', value=",pars$default[rows[i]],"))",newline)
        code=paste0(code,"          )",newline)
        code=paste0(code,"        )",ifelse(i==length(rows),"",","),newline)
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
      # Begin variable name
      code=paste0(code,"        fluidRow(",newline)
      code=paste0(code,"          column(12,",newline)
      code=paste0(code,"            div(style='",labStyle,
        "', '",paste0(vars$label[i],ifelse(as.logical(vars$steady[i]),' (SS)','')),"')",newline)
      code=paste0(code,"          )",newline)
      code=paste0(code,"        ),",newline)
      # End variable name
      code=paste0(code,"        fluidRow(",newline)
      code=paste0(code,"          column(5,",newline)
      code=paste0(code,"            div(style='",labStyle,
        "',textInput('",vars$name[i],
        "', label = '', value=",vars$default[i],"))",newline)
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
          column(1, div(style='",labStyle,"',textInput('.time.start',label = 'Start', value=",sett$.time.start,"))),
          column(1, div(style='",labStyle,"',textInput('.time.end', label = 'End', value=",sett$.time.end,"))),
          column(1, div(style='",labStyle,"',textInput('.time.dt', label = 'Step', value=",sett$.time.dt,"))),
          column(1, div(style='",labStyle,"',selectInput('.time.unit', label = 'Unit',
            choices=c('seconds','hours','days'), selected='",sett$.time.unit,"', multiple=FALSE, selectize=FALSE))),
          column(2, div(style='",labStyle,"',textInput('.time.base', label = 'Base (ISO 8601)', value='",sett$.time.base,"')))
        ),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Time axis')),
          column(1, div(style='",labStyle,"',textInput('.taxis.min', label = 'Min.', value=",sett$.taxis.min,"))),
          column(1, div(style='",labStyle,"',textInput('.taxis.max', label = 'Max.', value=",sett$.taxis.max,"))),
          column(1, div(style='",labStyle,"',checkboxInput('.taxis.grid', label='Grid', value=",sett$.taxis.grid,")))
        ),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Y axis')),
          column(1, div(style='",labStyle,"',textInput('.yaxis.min', label = 'Min.', value=",sett$.yaxis.min,"))),
          column(1, div(style='",labStyle,"',textInput('.yaxis.max', label = 'Max.', value=",sett$.yaxis.max,"))),
          column(1, div(style='",labStyle,"',checkboxInput('.yaxis.grid', label='Grid', value=",sett$.yaxis.grid,"))),
          column(1, div(style='",labStyle,"',checkboxInput('.yaxis.log', label='Log scale', value=",sett$.yaxis.log,")))
        ),
        fluidRow(
          column(2, p(style='",headStyle,"', 'Saved image (png)')),
          column(1, div(style='",labStyle,"',textInput('.png.width', label = 'Width (px)', value=",sett$.png.width,"))),
          column(1, div(style='",labStyle,"',textInput('.png.height', label = 'Height (px)', value=",sett$.png.height,"))),
          column(1, div(style='",labStyle,"',textInput('.png.res', label = 'Resol. (dpi)', value=",sett$.png.res,"))),
          column(3, div(style='",labStyle,"',textInput('.png.file', label = 'File name', value='",sett$.png.file,"')))
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

