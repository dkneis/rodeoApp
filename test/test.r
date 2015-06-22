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

#runGUI(dir="/home/dkneis/Dropbox/HS-Study-Project/intermediate_Model/Model_table_external")

runGUI(dir=system.file("examples/DRT", package="rodeoApp"))

#runGUI(dir="/home/dkneis/tudd/dynaTrait/claudia")
#runGUI(dir="/home/dkneis/tudd/dynaTrait/claudia", xlFile=NULL, colsep="\t")


#library(rodeoApp)
#runMCS(
#  outdir=gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE),
#  times=seq(0,30,1/24), nruns=10, nsig=4, overwrite=FALSE,
#  dir=system.file("examples/DRT", package="rodeoApp"))

