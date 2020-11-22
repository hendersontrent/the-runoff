#------------------------------------------
# This script sets out to load all things
# required for the various articles and
# analysis
#------------------------------------------

#------------------------------------------
# Author: Trent Henderson, 11 November 2020
#------------------------------------------

# Load packages

library(tidyverse)
library(data.table)
library(broom)
library(janitor)
library(fitzRoy)
library(scales)
library(Cairo)
library(ggpubr)
library(randomForest)
library(caret)
library(sjPlot)
library(caTools)
library(rstan)
library(bayesplot)
library(plotly)
library(ggrunoff) # Install using devtools::install_github("hendersontrent/ggrunoff")

# Turn off scientific notation

options(scipen = 999)

# Load functions

files <- list.files("R", full.names = TRUE, pattern = "\\.[Rr]")
for(f in files){
  source(f)
}

# Create a data folder for each topic if none exists:

if(!dir.exists('afl/data')) dir.create('afl/data')
if(!dir.exists('nrl/data')) dir.create('nrl/data')
if(!dir.exists('brownlow-forecasts/data')) dir.create('brownlow-forecasts/data')
if(!dir.exists('election-forecasts/data')) dir.create('election-forecasts/data')
if(!dir.exists('politics/data')) dir.create('politics/data')

# Create an output folder for each topic if none exists:

if(!dir.exists('afl/output')) dir.create('afl/output')
if(!dir.exists('nrl/output')) dir.create('nrl/output')
if(!dir.exists('brownlow-forecasts/output')) dir.create('brownlow-forecasts/output')
if(!dir.exists('election-forecasts/output')) dir.create('election-forecasts/output')
if(!dir.exists('politics/output')) dir.create('politics/output')