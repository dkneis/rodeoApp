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

