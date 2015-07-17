################################################################################
# Method to generate GUI code

ui_generate= function(vars, pars) {

  labStyle= "font-family: Arial Narrow; line-height: 10px; color: #3E566A"
  headStyle= "font-weight: bold; color: #3E566A"
  newline="\n"

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
          )
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
            ),
            fluidRow(
              column(2,
                div(style='",labStyle,"',textInput('time.min', label = 'min. time', value=0))
              ),
              column(2,
                div(style='",labStyle,"',textInput('time.max', label = 'max. time', value=10))
              ),
              column(2,
                div(style='",labStyle,"',textInput('time.dt', label = 'time step', value=1))
              )
            ),
            fluidRow(
              column(2,
                div(style='",labStyle,"',textInput('y.min', label = 'min. Y',
                  value=",min(vars$default)*ifelse(min(vars$default) < 0,2,0.5),"))
              ),
              column(2,
                div(style='",labStyle,"',textInput('y.max', label = 'max. Y',
                   value=",max(vars$default)*ifelse(max(vars$default) < 0,0.5,2),"))
              ),
              column(2,
                div(style='",labStyle,"',checkboxInput('logY', label='Log Y', value = FALSE))
              )
            ),
            fluidRow(
              column(2,
                div(style='",labStyle,"',actionButton('setRef', label='Set as ref.'))
              ),
              column(2,
                div(style='",labStyle,"',checkboxInput('showRef', label='Show ref.', value = FALSE))
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


# Alternative comment
################################################################################
# Create and execute GUI code

eval(parse(text=ui_generate(
  vars=get("rodeoApp.vars",envir=globalenv()),
  pars=get("rodeoApp.pars",envir=globalenv())
)))

