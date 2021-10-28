# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 30-03-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script retrieves the data on clutch size from the litterature
# and from the handbooks. The ouput is a master_dat.RData. Should be sourced with chdir = T. 
# This version is fixed for the major issue with the mean() function. 
# This version was moved to the new paper repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Paths
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_alhd = '../DATA/ALHD/Amniote-Life-History-Database.csv'
path_handbooks = '../DATA/other litterature/data.csv'
path_disko = '../DATA/DSKI/Data/Demographic_Database.csv'
path_out = '../RESULTS/clutch size/master_dat.RData'

# Load data
taxonomy = read.csv2(path_taxize, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
alhd = read.csv(path_alhd, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
handbooks = read.csv2(path_handbooks, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
disko = read.csv(path_disko)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Cleaning data from handbooks ----
handbooks$mean_clutch_size_n = sapply(1:nrow(handbooks), function(i) 
  mean(c(handbooks$min_n_eggs[i], handbooks$max_n_eggs[i]), na.rm = T))

# Cleaning data from disko ----
disko_orig = disko
disko = disko[! disko$varvalue %in% c('Y', '1?', 'yes', 'Yes', '3)', '2?'),]
disko$varvalue = as.numeric(disko$varvalue)
disko = pivot_wider(disko, names_from = demovar, values_from = varvalue, values_fn = mean) %>% as.data.frame
colnames(disko) = colnames(disko) %>% 
  str_replace_all(' ', '_') %>% 
  str_replace_all('/', '_') %>% 
  str_replace_all('-', '_')
disko = disko[is.na(disko$Litter_Clutch_size) | disko$Litter_Clutch_size < 100,]
lcsvars <- c('Clutch', 'X151LitterSize', 'litter_or_clutch_size_n',
             'largest.known.clutch',  'clutch.size', 'Litter.Clutch.size',
             'AdultFecundity', 'clutchSize.most.usual.max', 'clutchSize.most.usual.min',
             'clutchSize.mean', 'clutchSize.minimum.recorded',
             'clutchSize.maximum.recorded', 'Clutch.Size', 'Cs',
             'Ovoviviparous..number.of.eggs',
             'Viviparous..number.of.offspring',
             'Maximum.reproductive.output')
disko = disko[disko$varname %in% lcsvars,] # subsetting to clutch size, not clutches per year

# Taxize data frames ----
alhd = taxize.data.frame(alhd, 'Amniote.Life.History.Database.species', taxonomy)
handbooks = taxize.data.frame(handbooks, 'species', taxonomy, printer = F)
disko = taxize.data.frame(disko, 'species', taxonomy)

# Retrieve data ----
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat$clutch_size_n = sapply(master_dat$species, retrieve.variable,
                                  list(disko, alhd, handbooks), 
                                  c('Litter_Clutch_size', 'litter_or_clutch_size_n', 'mean_clutch_size_n'))

# Save data and print ----
save(master_dat, file = path_out)
print('Succesfully retrieved clutch sizes!')



