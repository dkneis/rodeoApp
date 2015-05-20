rm(list=ls())
setwd("/home/dkneis/tudd/dev/r_packages/rodeoApp/R")

xlFile= "../inst/examples/DRT/model.xlsx"

source("00_rodeoApp.r")
source("initModel.r")
source("plot.r")
source("runable.r")
source("simul.r")

library(shiny)
library(XLConnect)

sheets= c("vars", "pars")

# Read worksheets from Excel file and export tables to a dedicated environment
tryCatch({
  wBook= XLConnect::loadWorkbook(xlFile)
}, error= function(e) {
  stop(paste0("failed to import workbook from file '",xlFile,"'; details: ",e))
})
tbl= list()
for (i in 1:length(sheets)) {
  tryCatch({
    tmp= XLConnect::readWorksheet(object=wBook, sheet=sheets[i])
     # drop empty rows
    tmp= subset(tmp, apply(tmp, 1, function(x) {!all(is.na(x))}))
  }, error= function(e) {
    stop(paste0("failed to import data from worksheet '",sheets[i],
      "' of workbook (i.e. file) '",xlFile,"'; details: ",e))
  })
  tbl[[i]]= tmp
  names(tbl)[i]= sheets[i]
}


assign("rodeoApp.vars", tbl[["vars"]], envir=globalenv())

tbl[["pars"]]$user= as.logical(tbl[["pars"]]$user)
assign("rodeoApp.pars", tbl[["pars"]], envir=globalenv())


runApp("../inst/shiny")

