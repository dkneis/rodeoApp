rm(list=ls())

reinstall= FALSE

if (reinstall) remove.packages(c("rodeo","rodeoApp"))

if (!("devtools" %in% installed.packages()[,1]))
install.packages("devtools")
library("devtools")

if (!("rodeo" %in% installed.packages()[,1]))
install_github("dkneis/rodeo")
library("rodeo")

if (!("rodeoApp" %in% installed.packages()[,1]))
install_github("dkneis/rodeoApp")
library("rodeoApp")

#setwd("/home/dkneis/tudd/lehre/HSE")
#runGUI(dir="Model_external_2")

#obs= data.frame(1:5, T=seq(1, 1e9, length.out=5))
#runGUI(dir=system.file("examples/DRT", package="rodeoApp"), obs=obs)

runGUI(dir=system.file("examples/DRT", package="rodeoApp"))

#runGUI(dir=system.file("examples/DRT", package="rodeoApp"), dllname="myLib", serverMode=TRUE)

#runGUI(dir="/home/dkneis/tudd/dynaTrait/claudia")

#runGUI(dir="/home/dkneis/tudd/dev/r_packages/rodeoApp/inst/examples/chemostat")

#library(rodeoApp)
#runMCS(
#  outdir=gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE),
#  times=seq(0,30,1/24), nruns=10, nsig=4, overwrite=FALSE,
#  dir=system.file("examples/DRT", package="rodeoApp"))

