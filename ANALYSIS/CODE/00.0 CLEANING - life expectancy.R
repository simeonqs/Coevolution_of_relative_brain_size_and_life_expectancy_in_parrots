# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 01-04-2020
# Date last modified: 23-08-2021
# Author: Simeon Smeele
# Description: This script cleans the raw data from ZIMS, which will not be made available. It includes
# two steps:
# 1) CLEANING: cleaning the raw data based on exploratory analysis done by Fernando Colchero, this makes sure 
# to exclude individuals with impossible values (often data entry errors) - I rewrote part of this to make
# sure it includes more parrots 
# 2) ANALYSIS: standardising the taxonomy, removing contracepted individuals and subsetting for species with
# large enough sample size
# The output is basta.RData, which will not be made available, since it still contains sensitive information. 
# To reproduce the results, the ZIMS packages have to be installed manually from a file. 
# This version removes Fernando's cleaning. 
# This version removes sex and includes all species with a total of 50 individuals. 
# This version is moved to the new repo and paths are updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries, install the ZIMS packages manually using:
# install.packages(path_to_file, repos = NULL, type = 'source')
libraries = c('data.table', 'tidyverse', 'BaSTA.ZIMS', 'extractZIMS', 'snowfall')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Clean environment
rm(list = ls())

# Load functions
.functions = sapply(list.files('functions', full.names = T, pattern = '*.R'), source)

# Settings, change to suit your data
taxa = 'Aves'
ZIMSdir = '/Users/ssmeele/NOT_BACKED_UP/2021-01-08_new_data_birds/ZIMS.clean/ZIMSdata'
lowerDate = '1980-01-01' # lower date for the data
output_path = '/Users/ssmeele/NOT_BACKED_UP/2021-01-08_new_data_birds/simeon_bastadat/' 
taxonomy = read.csv2('../DATA/taxonomy/IUCN translation list - final.csv', stringsAsFactors = F) 
contracept_remove = 1 # 1 if you want to remove contracepted individuals, 0 if not
contracept_path = 
  paste0('/Users/ssmeele/NOT_BACKED_UP/2021-01-08_new_data_birds/ZIMS.clean/ZIMSdata/01.rawData/',
         '2020-01-22-Aves-Birds/sci_contraception_03d3a28435fc4b1b8667296a66c9841f.csv')
contracept_template_path = 
  paste0('/Users/ssmeele/NOT_BACKED_UP/2021-01-08_new_data_birds/ZIMS.clean/ZIMSdata/',
         '01.rawData/2020-01-22-Aves-Birds/template_contraception.csv')

# Load sub_taxonomy
sub_taxonomy = load.sub.taxonomy('../DATA/taxonomy/IUCN sub species taxonomy.csv')

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# CLEANING ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# List all the IUCN species that should be found
iucn_species = unique(na.omit(taxonomy$species))
nSp = length(iucn_species)

# Empty list to save bastadat per species
list_bastas = list()

# Load newcore to get unique binSpecies list
load(paste0(ZIMSdir, '/03.newCore/2020-01-22-Corrected-Aves.RData'))
binSpecies_unique = unique(newcore$binSpecies)

# Run through species
for(isp in 1:nSp) {
  
  # Species:
  sp = iucn_species[isp]
  
  # Find species names in ZIMS, this lists all binSpecies for current sp
  splist = taxonomy$original_species[which(taxonomy$species == sp)]
  splist = splist[splist %in% binSpecies_unique]
  if(length(splist) == 0){
    print(sprintf('Cleaning %s, but found no ZIMS species.', sp))
    next
  }
  
  # Print 
  print(sprintf('Cleaning %s, found these ZIMS species: %s.', sp, paste(splist, collapse = ', ')))

  # Subset the 'newcore' main table
  coresubset <- SubsetNewcore(taxa = taxa, ZIMSdir = ZIMSdir,
                              byTaxon = 'binSpecies', byTaxonLevs = splist,
                              exactMatch = TRUE)
  
  # Remove above 99% 
  coresubset = coresubset[coresubset$above99 == 0,]
  
  # Create BaSTA list
  bastalist <- CreateBastaDf(coresubset, earliestDate = "1980-01-01",
                             excludeStillbirth = T, excludeUnkSex = T,
                             printSummary = FALSE, excludeOutliers = FALSE)
  
  # Save basta in list
  list_bastas[[isp]] = bastalist$basta
  
} # End species loop

# Bind the bastadats
basta = bind_rows(list_bastas)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Taxize - this step also included subspecies
basta$original_species = basta$species
basta = taxize.data.frame(basta, 'original_species', taxonomy, remove_missing = T, printer = T,
                          incl_sub = T, sub_taxonomy = sub_taxonomy)

# Excluding contracepted individuals
if(contracept_remove == 1){
  contraception = fread(contracept_path, sep = '@')
  names(contraception) = names(fread(contracept_template_path, sep = '@'))
  dat = merge(basta, contraception, by.x = 'anonID', by.y = 'AnonID', all.x = T)
  remove = ifelse(str_detect(dat$Method, 'surgical'), dat$anonID, NA)
  remove = unique(remove, na.rm = T)
  basta = basta[! basta$anonID %in% remove,]
  print(paste0('Removed ', length(remove), ' individuals due to contraception.'))
}

# Test which species have enough individuals
include = c()
for(sp in unique(basta$species)){
  sub = basta[basta$species == sp,]
  if(nrow(sub) < 50) next
  include = c(include, sp)
}
basta = basta[basta$species %in% include,]

# Save basta
save(basta, file = paste0(output_path, 'basta_no_sex.RData'))

# Print 
print(sprintf('Processed all species. Saved %s with large enough sample.', length(unique(basta$species))))
