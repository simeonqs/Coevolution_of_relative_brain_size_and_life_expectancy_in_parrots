# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy 
# Date started: 08-06-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script calculates age of first reproduction (AFR). It uses raw data from Species360
# and can therefore not be reproduced. It takes the 5% percentile for species with N > 30. 
# This version changes the min to 6 months rather than 20 days. 
# This version was moved to the new repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA LOADING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'ape')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_core = paste0('/Users/ssmeele/NOT_BACKED_UP/2020-05-28_cleaned_data/Parrots/ZIMS.clean/ZIMSdata/',
                   '03.newCore/2020-01-22-Corrected-Aves.RData')
path_parent = paste0('/Users/ssmeele/NOT_BACKED_UP/2020-05-28_cleaned_data/Parrots/ZIMS.clean/ZIMSdata/',
                     '01.rawData/2020-01-22-Aves-Birds/sci_parent_03d3a28435fc4b1b8667296a66c9841f.csv')
path_temp_parent = paste0('/Users/ssmeele/NOT_BACKED_UP/2020-05-28_cleaned_data/Parrots/ZIMS.clean/', 
                          'ZIMSdata/01.rawData/2020-01-22-Aves-Birds/template_parent.csv')
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_alhd = '../DATA/ALHD/Amniote-Life-History-Database.csv'
path_out = '../RESULTS/AFR/5th percentile results.csv'
path_pdf = '../RESULTS/AFR/densities including sex.pdf'

# Load data
load(path_core)
core_orig = newcore
parent_orig = fread(path_parent, sep = '@', header = F)
names(parent_orig) = fread(path_temp_parent, sep = '@', header = F) %>% unlist
taxize = fread(path_taxize)
alhd = fread(path_alhd)
sub_taxonomy = load.sub.taxonomy('../DATA/taxonomy/IUCN sub species taxonomy.csv')

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA CLEANING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Taxonomy
core = core_orig
core$original_species = core$species
core = taxize.data.frame(core, 'original_species', taxonomy, remove_missing = T, printer = F,
                         incl_sub = T, sub_taxonomy = sub_taxonomy)

# Subset for parrots - already done
core = core[core$Order == 'Psittaciformes',]

# Only captive born with known or estimated birth date
core = core[core$birthType == 'Captive Birth/Hatch',]
core = core[! core$birthEst %in% c('Indeterminate', 'Undetermined'),]

# Check if birth estimate range is a problem - removing if more than 3 months
range_dates = as.numeric(core$MaxBirthDate - core$MinBirthDate)
core = core[range_dates < 90,]

# Only 100% certain parentage
parent = parent_orig
parent = parent[parent$Probability == 100,]

# Remove all indiviuals not in the core
parent = parent[parent$AnonID %in% core$anonID,]

# Remove all non-ZIMS parentages
parent = parent[parent$ParentOriginType == 'ZIMS',]

# Add birthdate of offspring to parent data frame
parent = merge(parent, core[,c('anonID', 'BirthDate')], 
               by.x = 'AnonID', by.y = 'anonID', all.x = T, all.y = F)

# Remove all inds from core that are not parents
core = core[core$anonID %in% parent$ParentAnonID,]

# Dates as dates
parent$BirthDate = parent$BirthDate %>% as.Date
core$BirthDate = core$BirthDate %>% as.Date

# Add age first reproduction in days to core
core$first.reproduction = NA
for(parent.id in unique(core$anonID)){
  
  # Subset parent 
  subparent = parent[parent$ParentAnonID == parent.id,]
  
  # Find first birth
  first = subparent[subparent$BirthDate ==  min(subparent$BirthDate),]
  
  # Enter first reproduction
  age.first.reproduction = first$BirthDate[1] - core$BirthDate[core$anonID == parent.id]
  core$first.reproduction[core$anonID == parent.id] = age.first.reproduction
  
} # End parent loop

# Remove ages of first reproduction below 6 months and above 80 years
core = core[core$first.reproduction > 6*30,]
core = core[core$first.reproduction < 365*80,]
core = as.data.frame(core)

# If sex is unknow, get it from the parentage file, else remove
core$Sex[!core$Sex %in% c('Male', 'Female')] = 
  sapply(core$anonID[!core$Sex %in% c('Male', 'Female')], function(x){
    y = NA
    if(all(parent$ParentType[parent$ParentAnonID == x] == 'Parentage_Dam')) y = 'Female'
    if(all(parent$ParentType[parent$ParentAnonID == x] == 'Parentage_Sire')) y = 'Female'
    return(y)
  }) 
core = core[!is.na(core$Sex),]

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Remove species with N < 30
or_size = nrow(core)
or_species = length(unique(core$species))
for(sp in unique(core$species)){
  sub = core[core$species == sp,]
  if(nrow(sub) < 30) core = core[core$sp != sp,]
}
print(paste0('Removed ', or_size - nrow(core), ' from ', or_size, ' individiuals.'))
print(paste0('Removed ', or_species - length(unique(core$species)), ' from ', or_species, ' species.'))

# Get 5th percentile
out = data.frame(species = unique(core$species))
out$afr_5th_percentile = sapply(out$species, function(x) 
  quantile(core$first.reproduction[core$species == x], 0.05))

# Write results
write.csv2(out, path_out)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# PLOTTING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

species = unique(core$species) %>% sort
pdf(path_pdf, 10, 10)
par(mfrow = c(4, 4))
for(sp in species){
  sub = core[core$species == sp,]
  plot(density(sub$first.reproduction/365, bw = 1), xlim = c(0, 30), main = sp)
  points(sub$first.reproduction/365, runif(nrow(sub), 0, 0.02), 
         col = alpha(c(Female = 'darkred', Male = 'darkblue')[sub$Sex], 0.5), pch = 16, cex = 1)
  abline(v = out$afr_5th_percentile[out$species == sp]/365, col = alpha('black', 0.5), lwd = 3, lty = 2)
}
dev.off()



