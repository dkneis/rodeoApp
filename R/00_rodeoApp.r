#' Run a rodeo-based model as a shiny application
#'
#' Type \code{help(package="rat")} to inspect the package description.
#'
#' @name rodeoApp-package
#' @docType package
{}

################################################################################
# Required packages

# Package for GUI
#' @import shiny
if (!("shiny" %in% installed.packages()[,1]))
  install.packages("shiny")
library(shiny)

# Package with numerical solvers
#' @import deSolve
if (!("deSolve" %in% installed.packages()[,1]))
  install.packages("deSolve")
library("deSolve")

# Package for reading Xl files
#' @import XLConnect
if (!("XLConnect" %in% installed.packages()[,1]))
  install.packages("XLConnect")
library("XLConnect")

# Package for latin hypercube sampling
#' @import lhs
if (!("lhs" %in% installed.packages()[,1]))
  install.packages("lhs")
library("lhs")

# Package allowing direct install of other packages from github
#' @import devtools
if (!("devtools" %in% installed.packages()[,1]))
  install.packages("devtools")
library("devtools")

# Package for code generation
#' @import rodeo
if (!("rodeo" %in% installed.packages()[,1]))
  install_github("dkneis/rodeo")
library("rodeo")

