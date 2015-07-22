#' Interfaces for rodeo-based models
#'
#' This package provides convenient interfaces for rodeo-based models. In
#' particular, it allows for running a rodeo-based from a (shiny) graphical user
#' interface (see \code{\link{runGUI}}). Type \code{help(package="rodeoApp")} to
#' inspect the package description.
#'
#'
#' @name rodeoApp-package
#' @aliases rodeoApp
#' @docType package
{}

################################################################################
# Required packages

# Package for GUI
#' @import shiny
if (!("shiny" %in% installed.packages()[,1]))
  install.packages("shiny")
library(shiny)

# Package with ODE integrators
#' @import deSolve
if (!("deSolve" %in% installed.packages()[,1]))
  install.packages("deSolve")
library("deSolve")

# Package with steady-state solvers
#' @import rootSolve
if (!("rootSolve" %in% installed.packages()[,1]))
  install.packages("rootSolve")
library("rootSolve")

# Package for reading XL files
#' @import readxl
if (!("readxl" %in% installed.packages()[,1]))
  install.packages("readxl")
library("readxl")

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

