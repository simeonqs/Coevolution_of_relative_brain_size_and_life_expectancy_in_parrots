# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 30-03-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script retrieves the data on incubation period and fledging period from the litterature
# and from the handbooks. The ouput is a master_dat.RData. Should be sourced with chdir = T. 
# This version was moved to the new paper repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'readxl')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_alhd = '../DATA/ALHD/Amniote-Life-History-Database.csv'
path_handbooks = '../DATA/other litterature/data.csv'
path_out = '../RESULTS/developmental time/master_dat.RData'

# Load data
taxonomy = read.csv2(path_taxize, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
alhd = read.csv(path_alhd, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
handbooks = read.csv2(path_handbooks, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Cleaning data from handbooks ----
handbooks$fledging_days = sapply(handbooks$fledging_days, function(x){
  if(is.na(x)) NA else {
    split = str_split(x, '-')
    mean(as.numeric(split[[1]]))
  }
})

# Cleaning data from alhd ----
alhd$incubation_d[alhd$incubation_d > 200] = NA # remove two extreme outliers
alhd$fledging_age_d[alhd$species == 'Conuropsis carolinensis'] = NA # outlier - extinct

# Standardise taxonomy ----
alhd = taxize.data.frame(alhd, 'Amniote.Life.History.Database.species', taxonomy)
handbooks = taxize.data.frame(handbooks, 'species', taxonomy, printer = T)

# Retrieve data ----
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat$fledging_age_days = sapply(master_dat$species, retrieve.variable, 
                                      list(alhd, handbooks, handbooks, handbooks), 
                                      c('fledging_age_d', 'fledging_days', 'nestling_period_days', 
                                        'fledging_period_days'))
master_dat$incubation_days = sapply(master_dat$species, retrieve.variable,
                                    list(alhd, handbooks), 
                                    c('incubation_d', 'incubation_days'))
# Save data
save(master_dat, file = path_out)

# Print
print('Succesfully retrieved incubation and fledging period!')