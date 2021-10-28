# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 01-04-2020
# Date last modified: 12-043-2021
# Author: Simeon Q. Smeele
# Description: This script runs BaSTA on the cluster and can be called multiple times using a batch
# script. The script will keep track of which species are being analysed by other threads. 
# It cannot be reproduced, since the data required cannot be shared. 
# It saves a PDF and RData file per species with the results from the model. 
# Note that only the GObt output was used in the final publication. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Clean
rm(list=ls())

# Paths
path_out = '/draco/u/simeonqs/basta/results no sex/' 
# path_out = '~/Desktop/'
path_basta = '/draco/u/simeonqs/basta/basta_no_sex.RData' 
# path_basta = '/NOT_BACKED_UP/2021-01-08_new_data_birds/simeon_bastadat/basta_no_sex.RData' 

# Libraries
library(snow)
library(snowfall)
library(BaSTA.ZIMS)
library(tidyverse)

# Models to run and names (CHANGE IF NEEDED):
modShape <- cbind(model = c("GO", "LO"), shape = c("bathtub", "bathtub"),
                  name = c("GObt", "LObt"))

# Number of models:
nMods <- nrow(modShape)

# Load data and set to start
load(path_basta)
basta_all = basta
species = unique(basta$species)
cont = T
first_run = T
ii = 0
Sys.sleep(runif(1, 0, 5)) # async the different versions

# Run all species
while(cont){ # continue until all species are done
  
  # Figure out which species to run
  ii = ii + 1 # updating index
  sp = species[ii]
  if(! any(str_detect(list.files(path_out, '.RData'), sp))){ # check if species is done
    if( ! (first_run  & any(str_detect(list.files(path_out, '.txt'), sp))) ){ # check if other chain already started
      
      # Make a txt to indicate that this chain started on this species and load data
      print(sprintf('Running species nr. %s: %s.', ii, sp))
      write.table('', paste0(path_out, sp, '.txt')) 
      basta = basta_all[basta_all$species == sp,]
      
      # List for model outputs:
      outBasta <- list()
      
      # Run models:
      for (mm in 1:nMods) {
        mName <- modShape[mm, "name"]
        outBasta[[mName]] <- bastaZIMS(basta, 
                                       shape = modShape[mm, "shape"], 
                                       model = modShape[mm, "model"],
                                       parallel = TRUE, ncpus = 4, 
                                       nsim = 4, niter = 80000, burnin = 15001, thinning = 30)
        
      } # End model loop
      
      # Save pdf
      pdf(file = sprintf("%s%s.pdf", path_out, sp), 7, 4)
      plot(outBasta$GObt)
      plot(outBasta$GObt, plot.type = "gof")
      plot(outBasta$GObt, plot.type = "demorates")
      plot(outBasta$LObt)
      plot(outBasta$LObt, plot.type = "gof")
      plot(outBasta$LObt, plot.type = "demorates")
      dev.off()
      
      # Save basta .RData file
      save("outBasta", file = sprintf("%s%s.RData", path_out, sp))
      file.remove(paste0(path_out, sp, '.txt')) # when done remove the starting file
      
    }  
  } 
  
  # Figure out how to continue
  if(first_run) if(ii >= length(species)){
    ii = 0
    first_run = F
  }  # if all species done, start over to check if any 
  # species are unfinished
  if(! first_run) if(ii >= length(species)) cont = F # if second run done, close the loop
}

