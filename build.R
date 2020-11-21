#---------------------------------------------------------
# This script drives the analysis for The Runoff from
# scratch by folder type
#
# NOTE: Any of these builds may take a long time to run
#---------------------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 21 November 2020
#------------------------------------------

source("setup.R")

#------------------------- MODELLING BUILDS ------------------------

# AFL

afl_scripts <- list.files("afl", pattern = "\\.[Rr]$", full.names = TRUE)

for(a in afl_scripts){
  source(a)
}

# NRL

nrl_scripts <- list.files("nrl", pattern = "\\.[Rr]$", full.names = TRUE)

for(n in nrl_scripts){
  source(n)
}

# Election forecasts

election_scripts <- list.files("election-forecasts", pattern = "\\.[Rr]$", full.names = TRUE)

for(e in election_scripts){
  source(e)
}

# Politics

pols_scripts <- list.files("politics", pattern = "\\.[Rr]$", full.names = TRUE)

for(p in pols_scripts){
  source(p)
}
