# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 15-10-2020
# Date last modified: 18-08-2021
# Author: Simeon Q. Smeele
# Description: This script cleans the weight data from ZIMS and outputs a RData file to be used in the next
# step. It also saves a plot per species to show which data is excluded. 
# NOTE: This script needs to be sourced with chdir = T. It also requires data that is not publicly available.
# It is only published for transparency and cannot be directly reproduced. 
# This version is updated to work in the new paper repo. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# SET-UP ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'extractZIMS', 'BaSTA.ZIMS', 'parallel', 'rstan')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Load functions
.functions = sapply(list.files('functions', pattern = '*.R', full.names = T), source)

# Paths and settings (raw ZIMS data is not made available)
taxa = 'Aves' # which order to clean up
lowerDate = '1980-01-01' # lower date for the data
ZIMSdir = '/Users/ssmeele/NOT_BACKED_UP/2021-01-08_new_data_birds/ZIMS.clean/ZIMSdata'
path_weights = 
  paste0(ZIMSdir, '/01.rawData/2020-01-22-Aves-Birds/sci_weights_03d3a28435fc4b1b8667296a66c9841f.csv')
path_template = 
  paste0(ZIMSdir, '/01.rawData/2020-01-22-Aves-Birds/template_weights.csv')
path_health = 
  paste0(ZIMSdir, '/01.rawData/2020-01-22-Aves-Birds/sci_health_03d3a28435fc4b1b8667296a66c9841f.csv')
path_template_health = 
  paste0(ZIMSdir, '/01.rawData/2020-01-22-Aves-Birds/template_health.csv')
path_taxonomy = '../DATA/taxonomy/IUCN translation list - final.csv'
path_sub_taxonomy = '../DATA/taxonomy/IUCN sub species taxonomy.csv'
path_out = '../RESULTS/weight/clean_dat.RData'
path_cleaning_results = '../RESULTS/weight/cleaning/cleaning '

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# CLEANING: loading ZIMS data, taxonomy and manual cleaning ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load data
LoadNewcore(taxa = taxa, ZIMSdir = ZIMSdir)
taxonomy = read.csv2(path_taxonomy, stringsAsFactors = F) 
sub_taxonomy = load.sub.taxonomy(path_sub_taxonomy)

# Taxize 
newcore$original_species = newcore$species
newcore = taxize.data.frame(newcore, 'original_species', taxonomy, remove_missing = T, printer = F,
                            incl_sub = T, sub_taxonomy = sub_taxonomy)

# Subset per species and create + save bastadat in list
basta_list = list()
for(sp in unique(newcore$species)){
  
  # Function to subset the 'newcore' main table
  coresubset <- SubsetNewcore.Simeon(taxa = taxa, ZIMSdir = ZIMSdir, 
                                     byTaxon = 'species',
                                     byTaxonLevs = sp,
                                     exactMatch = FALSE)
  
  # Create BaSTA table
  bastadat <- CreateBastaDf(coresubset, earliestDate = lowerDate,
                            excludeStillbirth = TRUE,
                            excludeOutliers = TRUE, outlierAlpha = 0.01, 
                            outlierFactor = 1, excludeUnkSex = TRUE)
  
  # Save in list
  basta_list[[sp]] = bastadat$basta
  
} # End sp loop

# Only include birds where we know birthdate
basta = bind_rows(basta_list, .id = 'species')
basta2 = basta[basta$Entry.Type == 'B',]

# Read weight data
weights = fread(path_weights, sep = '@')
names(weights) = names(fread(path_template, sep = '@'))

# Only keep individuals for which we have weights
basta2 = basta2[basta2$anonID %in% unique(weights$AnonID),]

# Clean weights
weights = weights[weights$AnonID %in% unique(basta$anonID),]
weights = weights[weights$ExcludedFromNorms == 0,]
weights = weights[weights$EstimatedMeasurement == 0,]
weights = weights[!weights$UnitOfMeasure %in% 
                    c('tonne', 'metre', 'grain', 'millimetre', 
                      'centimetre', '', 'degree fahrenheit', 'inch'),]
weights$MeasurementValue = ifelse(weights$UnitOfMeasure == 'kilogram', 
                                  weights$MeasurementValue * 1000, 
                                  weights$MeasurementValue)
weights$MeasurementValue = ifelse(weights$UnitOfMeasure == 'pound', 
                                  weights$MeasurementValue * 453.59237, 
                                  weights$MeasurementValue)
weights$MeasurementValue = ifelse(weights$UnitOfMeasure == 'ounce', 
                                  weights$MeasurementValue * 28.3495231, 
                                  weights$MeasurementValue)
weights$MeasurementValue = ifelse(weights$UnitOfMeasure == 'milligram', 
                                  weights$MeasurementValue / 1000, 
                                  weights$MeasurementValue)
weights$UnitOfMeasure = 'gram'
weights = weights[weights$MeasurementValue > 0.2,] # extremely low weights
weights = weights[weights$MeasurementValue < 10000,] # extremely high weights
# weights = weights[weights$AnonInstitutionID != 48142,] # poor data (multiples of 5)
weights = weights[weights$MeasurementType == 'Live weight',] # only live weights

# Getting age from core
weights$age_days = mclapply(1:nrow(weights), function(i){ # run through all rows
  sub = basta2[basta2$anonID == weights$AnonID[i],] # subset bastadata for that ID
  age = as.Date(weights$MeasurementDate[i]) - sub$Birth.Date[1] # take difference between birth and date
  return(as.numeric(age))
}, mc.cores = 6) %>% unlist

# Merge the two
dat = merge(weights, basta2, by.x = 'AnonID', by.y = 'anonID', all.x = T, all.y = F)

# Remove individuals that are not in basta2
dat = dat[!is.na(dat$species),]

# More cleaning
dat = as.data.frame(dat)
dat = dat[dat$age_days > 0,] # remove negative ages
dat$age_days_log = log(dat$age_days + 1e-6) # log the age 
dat = dat[c('AnonID', 'age_days', 'species', 'MeasurementValue', 'Sex', 'age_days_log', 
            'MeasurementDate', 'ExcludedFromNorms')]
dat$new_AnonID = as.integer(as.factor(dat$AnonID)) # oh, why did I ever do this
dat$row = 1:nrow(dat)

# Include information from health
health = fread(path_health, sep = '@')
names(health) = names(fread(path_template_health, sep = '@'))
health$Date = as.Date(health$Date)

# Run through individuals
for(id in unique(dat$AnonID)){
  
  # Skip if no health info
  if(!id %in% health$AnonID) next
  
  # Subset
  sub_dat = dat[dat$AnonID == id,]
  sub_health = health[health$AnonID == id,]
  
  # Run through problems
  problems = c()
  for(i in which(sub_health$Status != 'Normal')){
    start = sub_health$Date[i]
    end = ifelse(i == nrow(sub_health),
                 as.Date('2050-01-01'),
                 sub_health$Date[i+1])
    problems = c(problems, which(sub_dat$MeasurementDate > start & sub_dat$MeasurementDate < end))
    dat = dat[!dat$row %in% sub_dat$row[problems],]
  }
  
} # End id loop

# Manual cleaning data
dat = dat[!(dat$species == 'Pyrrhura hoffmanni' & dat$MeasurementValue > 600),] # one extreme value
dat = dat[!(dat$species == 'Ara macao' & dat$MeasurementValue < 250 & 
              dat$age_days_log > 6),] # many low values
dat = dat[!dat$new_AnonID == 5893,] # remove two individuals that do not seem part of this species Ara severus
dat = dat[!dat$new_AnonID == 7230,]
dat = dat[!dat$new_AnonID == 16297,] # same for Psitteuteles iris
dat = dat[!dat$new_AnonID == 17667,] # same for Nestor meridionalis
dat = dat[!dat$new_AnonID == 19879,] 
dat = dat[!dat$new_AnonID == 9891,] # Neophema chrysogaster
dat = dat[!dat$new_AnonID == 301,] # Forpus spengeli
dat = dat[!dat$new_AnonID == 31811,] # Probosciger aterrimus
dat = dat[!(dat$MeasurementValue == 35 & dat$species == 'Psilopsiagon aurifrons'),]

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# CLEANING: removing juveniles and outliers ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Empty data frame to save clean data
clean_dat = data.frame()

# Loop through species to clean data
for(species in unique(dat$species)){
  
  # Tell me where you are
  print(paste0("Doing ", which(unique(dat$species) == species), ' out of ', 
               length(unique(dat$species)), '.'))
  
  # Subset dat
  subdat = dat[dat$species == species,]
  
  # Remove extreme outliers first
  remove0 = which(subdat$MeasurementValue > mean(subdat$MeasurementValue) + 4*sd(subdat$MeasurementValue))
  if(length(remove0) > 0) subdat = subdat[-remove0,]
  
  # Remove outliers
  removed = subdat[subdat$age_days_log < 0.5*max(subdat$age_days_log),] # remove juveniles
  subdat = subdat[subdat$age_days_log > 0.5*max(subdat$age_days_log),]
  k = 3 # setting for outlier cleaning
  if(species == 'Probosciger aterrimus') k = 1 # better setting for one species
  if(length(unique(subdat$AnonID)) > 2) # include one value per individual, if more than 2 individuals
                                        # only used for quantiles, all values included in the final data
    dupies = !duplicated(subdat$AnonID) else dupies =  rep(T, nrow(subdat))
  # Fix for problem species
  if(species %in% c('Aprosmictus jonquillaceus', 'Psittacula eques', 
                    'Tanygnathus lucionensis')) dupies =  rep(T, nrow(subdat))
  q = quantile(log(subdat$MeasurementValue[dupies])) # find quantiles, using only one value per id
  low = as.numeric(q[2] - k * (q[4] - q[2])) # set upper and lower bound
  high = as.numeric(q[4] + k * (q[4] - q[2]))
  removed = rbind(removed, # clean the outliers
                  subdat[log(subdat$MeasurementValue) < low | log(subdat$MeasurementValue) > high,])
  subdat = subdat[!(log(subdat$MeasurementValue) < low | log(subdat$MeasurementValue) > high),]
  
  # Plot overview of cleaned and original data
  pdf(paste0(path_cleaning_results, species, '.pdf'), 5, 5)
  plot(subdat$age_days_log, subdat$MeasurementValue, 
       ylim = c(0, max(removed$MeasurementValue, subdat$MeasurementValue)),
       xlim = c(0, max(subdat$age_days_log)),
       col = 3, pch = 16, xlab = 'log age [d]', ylab = 'weight [g]', cex = 0.5)
  points(removed$age_days_log, removed$MeasurementValue, col = 2, pch = 16, cex = 0.5)
  lines(rep(max(subdat$age_days_log)/2, 2), c(0, max(subdat$MeasurementValue)), lty = 2)
  text(0, 0.05*max(subdat$MeasurementValue), paste0('N_ind = ', length(unique(subdat$AnonID))),
       adj = 0)
  dev.off()
  
  # Save data 
  clean_dat = rbind(clean_dat, subdat)
  
} # End species loop

# Save clean_dat
save(clean_dat, file = path_out)
