# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 29-06-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script analyses the raw brain volumes collected by Andrew and the data taken from 
# Ksepka et al. 2020. It uses a multilevel Bayesian model to get average adult brain size per species and saves
# this including SE in master_dat.RData. 
# After summarising the raw data it includes additional data (for species with no average) from 
# Schuck-Paim et al. 2008 and Sayol et al. 2020. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_data = '../DATA/Andrew Iwaniuk/data.csv'
path_ksep = '../DATA/Ksepka et al. 2020/dat.csv'
path_taxonomy = '../DATA/taxonomy/IUCN translation list - final.csv'
path_schuck = '../DATA/Schuck-Paim et al. 2008/data.csv'
path_sayol = '../DATA/Sayol et al. 2020/SpeciesTraits.csv'
path_model = '../RESULTS/brain/model.RData'
path_pdf = '../RESULTS/brain/compare mean to raw data.pdf'
path_out = '../RESULTS/brain/master_dat.RData'

# Load data
dat_orig = read.csv2(path_data, na.strings = c('', ' ', 'NA'), stringsAsFactors = F, header = F)
ksep_orig = read.csv2(path_ksep, na.strings = c('', ' ', 'NA', '"'), stringsAsFactors = F, dec = ',')
taxize = read.csv2(path_taxonomy, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)

# Combining species and naming columns, and filling out species
names(dat_orig) = 
  c('genus', 'species_latin', 'ID', 'sex', 'no_idea', 'brain', 'more_no_idea', 'final_no_idea')
for(i in 1:nrow(dat_orig)){
  if(dat_orig$species_latin[i] %>% is.na) dat_orig$species_latin[i] = dat_orig$species_latin[i-1]
  if(dat_orig$genus[i] %>% is.na) dat_orig$genus[i] = dat_orig$genus[i-1]
}
dat_orig$species_andrew = paste(dat_orig$genus, dat_orig$species_latin)
for(i in 1:nrow(ksep_orig)){
  if(ksep_orig$Clade[i] %>% is.na) ksep_orig$Clade[i] = ksep_orig$Clade[i-1]
  if(ksep_orig$Taxon[i] %>% is.na) ksep_orig$Taxon[i] = ksep_orig$Taxon[i-1]
}
ksep_orig = ksep_orig[ksep_orig$Clade == 'Psittaciformes',]

# Remove anything genus or above
dat_orig$species_andrew = sapply(dat_orig$species_andrew, function(x){
  split = strsplit(x, " ")[[1]]
  if(length(split) < 2) y = "remove"
  if(length(split) == 2) y = x
  if(length(split) > 2) y = paste(split[1], split[2])
  return(y)
})
dat_orig = dat_orig[dat_orig$species_andrew != "remove",]

ksep_orig$Taxon = sapply(ksep_orig$Taxon, function(x){
  split = strsplit(x, " ")[[1]]
  if(length(split) < 2) y = "remove"
  if(length(split) == 2) y = x
  if(length(split) > 2) y = paste(split[1], split[2])
  return(y)
})
ksep_orig = ksep_orig[ksep_orig$Taxon != "remove",]

# Merge taxize on dat to ensure name consistency
dat = merge(dat_orig, taxize[c('original_species', 'species')], 
            by.x = 'species_andrew', by.y = 'original_species', all.x = T, all.y = F)
ksep = merge(ksep_orig, taxize[c('original_species', 'species')], 
             by.x = 'Taxon', by.y = 'original_species', all.x = T, all.y = F)

# Find problems
problems = dat[dat$species %>% is.na,] # for now no problems left
if(nrow(problems) > 0){
  print('These species from Andrew are not found in the taxize list:')
  print(unique(problems$species_andrew))
}
problems = ksep[ksep$species %>% is.na,] # for now no problems left
if(nrow(problems) > 0){
  print('These species from Ksep are not found in the taxize list:')
  print(unique(problems$Taxon))
}

# Remove species that are not found
dat = dat[!is.na(dat$species),]
ksep = ksep[!is.na(ksep$species),]

# Merge ksep on dat
ksep.new = data.frame(species = ksep$species, 
                      sex = NA,
                      brain = ksep$Endocranial.Volume/1036 )
dat = rbind(dat[c('species', 'sex', 'brain')], ksep.new)
dat$sex[! dat$sex %in% c('f', 'm')] = 'u'

# Load taxonomy and other sources
taxonomy = read.csv2(path_taxonomy, 
                     na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
schuck_paim = read.csv2(path_schuck, stringsAsFactors = F)
sayol = read.csv2(path_sayol, stringsAsFactors = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Model
species = unique(dat$species)
species_translate = data.frame(x = 1:length(species))
rownames(species_translate) = species
clean_dat = list(species = species_translate[dat$species,],
                 log_brain_mass = log(dat$brain),
                 N_species = length(species),
                 N_obs = nrow(dat))

model = ulam(
  alist(
    log_brain_mass ~ normal(mu, sigma),
    mu <- mu_species[species],
    mu_species[species] ~ normal(m_mu, sd_mu),
    sd_mu ~ exponential(1),
    m_mu ~ normal(2, 1),
    sigma ~ exponential(1)
  ), data = clean_dat, chains = 4, cores = 4)

# trankplot(model)
precis(model, depth = 1)
post = extract.samples(model)

# Test R hat
t = precis(model, depth = 2)
rhat_max = round(t@.Data[[6]], 2) %>% max
print(sprintf('Max rhat is %s.', rhat_max))

# Save model for later
save(model, file = path_model)

# Plot estimate on raw data
pdf(path_pdf, 15, 5)
plot(clean_dat$species,
     clean_dat$log_brain_mass,
     pch = 16, col = alpha('darkgreen', 0.7),
     xlab = 'species index', ylab = 'log brain mass [g]')
points(1:max(clean_dat$species),
       apply(post$mu_species, 2, mean),
       pch = 16, col = alpha('red', 0.9))
pi = apply(post$mu_species, 2, PI, prob = 0.95)
for(i in 1:max(clean_dat$species)) lines(rep(i, 2),
                                         c(pi[1,i],
                                           pi[2,i]),
                                         lwd = 3, col = alpha('red', 0.2))
dev.off()

# Adding data from model
model_out = data.frame(species = rownames(species_translate),
                       log_mean_brain_size = apply(post$mu, 2, mean),
                       log_SE_brain_size = apply(post$mu, 2, sd))
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat = merge(master_dat, model_out, by = 'species', all.x = T, all.y = F)
na_1 = length(which(is.na(master_dat$log_mean_brain_size)))

# Add data from other sources
schuck_paim$n_brain[is.na(schuck_paim$n_brain)] = 1
schuck_paim = taxize.data.frame(schuck_paim, 'species_original', taxonomy = taxonomy, printer = T)
sayol$species_original = str_replace(sayol$Species, '_', ' ')
sayol = taxize.data.frame(sayol, 'species_original', printer = F, taxonomy = taxonomy)
## adding data from Schuck-paim
for(sp in master_dat$species[is.na(master_dat$log_mean_brain_size)]){
  bm = schuck_paim$brain_vol[schuck_paim$species == sp]/1.036 # volume to mass
  if(length(bm) == 0) next
  master_dat$log_mean_brain_size[master_dat$species == sp] = log(bm)
}
## adding data from Sayol
for(sp in master_dat$species[is.na(master_dat$log_mean_brain_size)]){
  bm = sayol$BrainSize[sayol$species == sp]
  if(length(bm) == 0) next
  master_dat$log_mean_brain_size[master_dat$species == sp] = log(bm)
}
## adding SE from Schuck-Paim, using SD =  0.1 from the model
for(sp in master_dat$species[is.na(master_dat$log_SE_brain_size)]){
  n = schuck_paim$n_brain[schuck_paim$species == sp]
  if(length(n) == 0) next
  master_dat$log_SE_brain_size[master_dat$species == sp] = 0.1/sqrt(n)
}
## adding SE from Sayol
for(sp in master_dat$species[is.na(master_dat$log_SE_brain_size)]){
  n = sayol$BrainSampleSize[sayol$species == sp]
  if(length(n) == 0) next
  master_dat$log_SE_brain_size[master_dat$species == sp] = 0.1/sqrt(n)
}
na_2 = length(which(is.na(master_dat$log_mean_brain_size)))

# Save the averages per species
save(master_dat, file = path_out)

# Print
print(sprintf('Analysed %s species and added data from other sources for %s more.',
              clean_dat$N_species, na_1-na_2))
