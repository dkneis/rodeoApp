
################################################################################
# Method to generate GUI code

ui_generate= function(vars, pars) {
  labStyle= "font-family: Arial Narrow; line-height: 10px; color: #3E566A"
  headStyle= "font-weight: bold; color: #3E566A"
  newline="\n"
  code="# THIS IS A GENERATED FILE -- DO NOT EDIT"
  code=paste0(code,newline,"",newline)
  code=paste0(code,"shinyUI(",newline)
  code=paste0(code,"  fluidPage(",newline)
  code=paste0(code,"    fluidRow(",newline)
  code=paste0(code,"      column(2,",newline)
  code=paste0(code,"        p(style='",headStyle,"', 'Parameters')",newline)
  code=paste0(code,"      ),",newline)
  code=paste0(code,"      column(2,",newline)
  code=paste0(code,"        p(style='",headStyle,"', 'Variables'),",newline)
  code=paste0(code,"        fluidRow(",newline)
  code=paste0(code,"          column(5,",newline)
  code=paste0(code,"            p(style='",headStyle,"', 'Init.')",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(4,",newline)
  code=paste0(code,"            p(style='",headStyle,"', 'Mult.')",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(3,",newline)
  code=paste0(code,"            p(style='",headStyle,"', 'On')",newline)
  code=paste0(code,"          )",newline)
  code=paste0(code,"        )",newline)
  code=paste0(code,"      )",newline)
  code=paste0(code,"    ),",newline)
  code=paste0(code,"    fluidRow(",newline)
  # Parameters
  rows= which(pars$user)
  code=paste0(code,"      column(2,style = 'overflow-y:scroll; max-height: 800px',",newline)
  if (length(rows) > 0) {
    for (i in rows) {
      code=paste0(code,"      div(style='",labStyle,"',textInput('",pars$name[i],"', label = '",
        pars$label[i],"', value=",pars$default[i],"))",
        ifelse(i==rows[length(rows)],"",","),newline)
    }
  } else {
    code=paste0(code,"p('No visible items')",newline)
  }
  code=paste0(code,"      ),",newline)
  # Variables
  code=paste0(code,"      column(2,style = 'overflow-y:scroll; max-height: 800px',",newline)
  for (i in 1:nrow(vars)) {
    code=paste0(code,"        fluidRow(",newline)
    code=paste0(code,"          column(5,",newline)
    code=paste0(code,"            div(style='",labStyle,"',textInput('",vars$name[i],
      "', label = '",vars$label[i],"', value=",vars$default[i],"))",newline)
    code=paste0(code,"          ),",newline)
    code=paste0(code,"          column(4,",newline)
    code=paste0(code,"            div(style='",labStyle,"',textInput('",vars$name[i],
      ".mult', label = ' ', value=1))",newline)
    code=paste0(code,"          ),",newline)
    code=paste0(code,"          column(3,",newline)
    code=paste0(code,"            div(style='",labStyle,"',checkboxInput('",vars$name[i],
      ".draw', label = ' ', value=TRUE))",newline)
    code=paste0(code,"          )",newline)
    code=paste0(code,"        )",ifelse(i==nrow(vars),"",","),newline)
  }
  code=paste0(code,"      ),",newline)

  code=paste0(code,"      column(8,",newline)
  code=paste0(code,"        fluidRow(",newline)
  code=paste0(code,"          column(12,",newline)
  code=paste0(code,"            plotOutput(outputId='plotStoi', height='150px', width='50%'),",newline)
  code=paste0(code,"            plotOutput(outputId='plotStates', height='600px', width='90%')",newline)
  code=paste0(code,"          )",newline)
  code=paste0(code,"        ),",newline)
  code=paste0(code,"        fluidRow(",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',textInput('time.max', label = 'max. time', value=10))",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',textInput('time.dt', label = 'time step', value=1))",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',textInput('y.min', label = 'min. Y', value=",
    min(vars$default)*ifelse(min(vars$default) < 0,2,0.5),"))",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',textInput('y.max', label = 'max. Y', value=",
    max(vars$default)*ifelse(max(vars$default) < 0,0.5,2),"))",newline)
  code=paste0(code,"          )",newline)
  code=paste0(code,"        ),",newline)
  code=paste0(code,"        fluidRow(",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',actionButton('setRef', label='Set as ref.'))",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(2,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',checkboxInput('showRef', label='Show ref.', value = FALSE))",newline)
  code=paste0(code,"          ),",newline)
  code=paste0(code,"          column(4,",newline)
  code=paste0(code,"            div(style='",labStyle,
    "',checkboxInput('logY', label='Log Y', value = FALSE))",newline)
  code=paste0(code,"          )",newline)
  code=paste0(code,"        )",newline)
  code=paste0(code,"      )",newline)
  code=paste0(code,"    )",newline)
  code=paste0(code,"  )",newline)
  code=paste0(code,")",newline)

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

eval(parse(text=ui_generate(vars=get("rodeoApp.vars",envir=globalenv()),
  pars=get("rodeoApp.pars",envir=globalenv()))))

