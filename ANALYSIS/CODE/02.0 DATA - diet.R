# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 30-03-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script takes the diet data collected from the literature and from researchers and 
# organises it. It saves a master_dat.RData in the DIET folder. 
# This version uses the combined excel sheet. 
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
path_taxonomy = '../DATA/taxonomy/IUCN translation list - final.csv'
path_diet = '../DATA/diet/diet data.xlsx'
path_out = '../RESULTS/diet/master_dat.RData'

# Load data
taxonomy = read.csv2(path_taxonomy, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
diet_master = read_xlsx(path_diet, sheet = 1)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Get protein content
## if a high protein item is a main diet item - high
## if the only main items are low protein - low
## otherwise - medium
diet_master = as.data.frame(diet_master)
diet_master$protein = NA
for(i in 1:nrow(diet_master)){
  sub = diet_master[i,]
  low = sub[c('nectar', 'fruit', 'leaves/shoots/buds (shrubs & trees)',
              'leaves/shoots/buds (ground-cover)', 'bulbs/roots')]
  high = sub[c('seeds', 'nuts', 'meat')]
  if(! 'main' %in% low) diet_master[i,]$protein = 3 else 
    if(! 'main' %in% high) diet_master[i,]$protein = 1 else
      diet_master[i,]$protein = 2
  if(c(low, high) %>% is.na %>% all) diet_master[i,]$protein = NA
}

# Get diet extraction
diet_master$extraction = NA
for(i in 1:nrow(diet_master)){
  sub = diet_master[i,]
  high = sub[c('nuts', 'meat', 'bulbs/roots')]
  if('main' %in% high | 'extra'%in% high) diet_master[i,]$extraction = 2 else 
    diet_master[i,]$extraction = 1
  if(sub[c('nectar', 'fruit', 'leaves/shoots/buds (shrubs & trees)',
           'leaves/shoots/buds (ground-cover)', 'bulbs/roots',
           'seeds', 'nuts', 'meat')] %>% is.na %>% all) diet_master[i,]$extraction = NA
}

# Get diet breadth
diet_master$breadth = NA
for(i in 1:nrow(diet_master)){
  sub = diet_master[i,c('nectar', 'fruit', 'leaves/shoots/buds (shrubs & trees)',
                         'leaves/shoots/buds (ground-cover)', 'seeds', 'nuts', 'meat', 
                         'bulbs/roots')]
  score = c(no = 0, extra = 1, main = 2)[as.character(sub)]
  diet_master[i,]$breadth = sum(score)
}

# Explore it
if(F){
  pdf('~/Desktop/diet.pdf')
  dens(diet_master$breadth, main = 'diet breadth')
  plot(diet_master$protein + runif(nrow(diet_master), -0.2, 0.2), 
       diet_master$breadth, 
       xlab = 'protein', ylab = 'breadth')
  plot(diet_master$extraction + runif(nrow(diet_master), -0.2, 0.2), 
       diet_master$breadth, 
       xlab = 'extraction', ylab = 'breadth')
  plot(diet_master$extraction + runif(nrow(diet_master), -0.2, 0.2), 
       diet_master$protein + runif(nrow(diet_master), -0.2, 0.2), 
       xlab = 'extraction', ylab = 'protein')
  dev.off()
}
  
# Make sure taxonomy is still correct
diet_master = taxize.data.frame(diet_master, 'species', taxonomy)

# Save relevant data
master_dat = data.frame(species = diet_master$species, 
                        diet_protein = diet_master$protein)
save(master_dat, file = path_out)

# Print
print('Processed diet data succesfully!')


