# BEGIN SETTINGS ###############################################################

library(rodeoApp)

# Select analysis example by ID
exampleID= 1

# Set work folder
wdir= gsub(pattern="\\", replacement="/", x=tempdir(), fixed=TRUE)

# Sampling and simulations
runMCS(outdir=wdir, times=seq(0,90,1/4), nruns=100, nsig=4, overwrite=TRUE,
  dir=system.file("examples/DRT", package="rodeoApp"))

# Read outputs
var= read.table(file=paste0(wdir,"/var.txt"), header=T, sep="\t")
par= read.table(file=paste0(wdir,"/par.txt"), header=T, sep="\t")
sim= read.table(file=paste0(wdir,"/sim.txt"), header=T, sep="\t")

# Merge tables for analysis
sim= merge(x=sim, y=var, by="runID", suffixes=c("","_initial"))
sim= merge(x=sim, y=par, by="runID")
rm(var, par)

# Example 1: Percentage of transconj. at end of simulation period
if (exampleID == 1) {
  tmax= max(sim$time)
  sim$DRT= sim$D + sim$R + sim$T
  tmp= subset(sim, sim$time == tmax)
  layout(matrix(1:3, ncol=3))
  xlab="Antibiotic"
  ylim=c(0,100)
  plot(tmp$A, tmp$D/tmp$DRT*100, xlab=xlab, ylim=ylim, ylab="Donors at final time (%)")
  plot(tmp$A, tmp$R/tmp$DRT*100, xlab=xlab, ylim=ylim, ylab="Recip. at final time (%)")
  plot(tmp$A, tmp$T/tmp$DRT*100, xlab=xlab, ylim=ylim, ylab="Trans. at final time (%)")

# Example 2: Maximum abundance of a group and corresponding timing
} else if (exampleID == 2) {
  # Find rows where abundance of R is highest
  tmax= tapply(X=sim$R, INDEX=sim$runID, FUN=which.max)  
  tlen= tapply(X=sim$R, INDEX=sim$runID, FUN=length)
  rows= c(0,cumsum(tlen[-length(tlen)])) + tmax
  # Select these rows
  tmp= sim[rows,]
  # Plots
  layout(matrix(1:2, ncol=2))
  plot(tmp$A, tmp$R, xlab="Antibiotic", ylab="Max. recip. abundance")
  plot(tmp$A, tmp$time, xlab="Antibiotic", ylab="Time of max. recip. abundance")

} else {
  print("no such example")
}

