# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 30-03-2020
# Date last modified: 28-06-2021
# Author: Simeon Q. Smeele
# Description: This script organises the insularity data from different sources. 
# Should be sourced using chdir = T. 
# This version was moved to the new paper repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'taxize', 'readxl')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_sayol = '../DATA/Sayol et al. 2020/SpeciesTraits.csv'
path_handbooks = '../DATA/other litterature/data.csv'
path_ducatez = '../DATA/Ducatez et al. 2020/Dataset_Ducatez_et_al.csv'
path_out = '../RESULTS/insularity/master_dat.RData'

# Load data
taxonomy = read.csv2(path_taxize, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
sayol = read.csv2(path_sayol, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
handbooks = read.csv2(path_handbooks, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
ducatez = read.csv2(path_ducatez, na.strings = c('', ' ', 'NA'))

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>


# Cleaning data from sayol ----
sayol$Insularity = c(insular = 1, continental = 0)[sayol$Insularity]
sayol$Species = str_replace_all(sayol$Species, '_', ' ')

# Cleaning Ducatez ----
ducatez$species = ducatez$Species %>% str_replace('_', ' ')

# Standardise taxonomy ----
handbooks = taxize.data.frame(handbooks, 'species', taxonomy)
sayol = taxize.data.frame(sayol, 'Species', taxonomy)
ducatez = taxize.data.frame(ducatez, 'species', taxonomy)

# Retrieve data ----
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat$insularity = sapply(master_dat$species, retrieve.variable, 
                               list(sayol, # seems legit, still also large islands
                                    ducatez, # seems legit
                                    handbooks), # manually checked, excluded Australia
                               c('Insularity', 
                                 'Insularity', 
                                 'insularity'), type = 'categorical')

# Write the output
save(master_dat, file = path_out)

# Print
print('Processed insularity!')
