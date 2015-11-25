rm(list=ls())

idir= "/home/dkneis/tudd/dev/r_packages/rodeoApp/inst/examples/chemostat"
odir= paste0(idir,"/tex")

################################################################################

# Load model
library(rodeo)
library(rodeoApp)
model= initModel(dir=idir)$model

# Low-level formatting functions
firstUpper= function(x) {
  paste0(toupper(substr(x,1,1)), substr(x,2,nchar(x)))
}
bold= function(x) { paste0("\\textbf{",x,"}") }
slanted= function(x) { paste0("\\textsl{",x,"}") }
allow_= function(x) { gsub(pattern="_", replacement="\\_",
  x=x, fixed=TRUE) }

# High-level formatting functions
niceHead= function(x) { bold(firstUpper(x)) }
math= function(x) { paste0("$",gsub(pattern="*", replacement="\\cdot ",
  x=x, fixed=TRUE),"$") }
signsymbol= function(x) {
  if (as.numeric(x) > 0) return("\\textcolor{orange}{$\\blacktriangle$}")
  if (as.numeric(x) < 0) return("\\textcolor{cyan}{$\\blacktriangledown$}")
  return("")
}


tbl= model$getVars()[,c("tex","unit","description")]
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= c(tex="Symbol"),
  funHead= setNames(replicate(ncol(tbl),niceHead), names(tbl)),
  funCell= c(tex=math, description=firstUpper)
), file=paste0(odir,"/vars.tex"))


tbl= model$getPars()[,c("tex","unit","description")]
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= c(tex="Symbol"),
  funHead= setNames(replicate(ncol(tbl),niceHead), names(tbl)),
  funCell= c(tex=math, description=firstUpper)
), file=paste0(odir,"/pars.tex"))


tbl= model$getFuns()[,c("tex","unit","description")]
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= c(tex="Symbol"),
  funHead= setNames(replicate(ncol(tbl),niceHead), names(tbl)),
  funCell= c(tex=math, description=firstUpper)
), file=paste0(odir,"/funs.tex"))


tbl= model$getPros()[,c("name","unit","description","expression_tex")]
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= c(expression_tex="rate expression"),
  funHead= setNames(replicate(ncol(tbl),niceHead), names(tbl)),
  funCell= c(description=firstUpper, expression_tex=math)
), file=paste0(odir,"/pros.tex"))


tbl= model$getStoi()[,c("variable_tex","process","expression_tex")]
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= c(variable_tex="variable", expression_tex="factor"),
  funHead= setNames(replicate(ncol(tbl),niceHead), names(tbl)),
  funCell= c(variable_tex=math, expression_tex=math),
  align= c(expression_tex="r")
), file=paste0(odir,"/stoi.tex"))


# Values
pars= setNames(as.numeric(model$getPars()$default), model$getPars()$name)
vars= setNames(as.numeric(model$getVars()$default), model$getVars()$name)

monod= function(s,h) {s / (s + h)}

# Computes the stoichiometry matrix
m= model$stoichiometryMatrix(c(vars, pars, time=0))
tbl= cbind(data.frame(process=rownames(m), stringsAsFactors=FALSE), as.data.frame(m))
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= setNames(c("Process",model$getVars()$tex[match(colnames(m),
    model$getVars()$name)]), names(tbl)),
  funHead= setNames(replicate(ncol(tbl),math), names(tbl)),
  funCell= setNames(replicate(ncol(m),math), colnames(m))
), file=paste0(odir,"/stoi_num.tex"))


# Symbolic stoichiometry matrix
m= model$stoichiometryMatrix(c(vars, pars, time=0))
tbl= cbind(data.frame(process=rownames(m), stringsAsFactors=FALSE), as.data.frame(m))
write(x=exportDF(x=tbl, tex=TRUE,
  colnames= setNames(c("Process",model$getVars()$tex[match(colnames(m),
    model$getVars()$name)]), names(tbl)),
  funHead= setNames(replicate(ncol(tbl),math), names(tbl)),
  funCell= setNames(replicate(ncol(m),signsymbol), colnames(m))
), file=paste0(odir,"/stoi_sym.tex"))

