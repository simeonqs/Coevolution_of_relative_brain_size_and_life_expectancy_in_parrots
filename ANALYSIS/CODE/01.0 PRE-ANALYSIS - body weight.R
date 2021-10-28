# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 15-10-2020
# Date last modified: 24-05-2021
# Author: Simeon Q. Smeele
# Description: This script uses the cleaned weight data from '00 CLEANING - body weight.R'. It also includes
# data from the literature for the species for which we have no captive data. It runs a multilevel
# Bayesian model to estimate average adult body weight per species and saves this including the 
# SE in master_dat.RData. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'parallel', 'rstan')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Load functions
.functions = sapply(list.files('functions', pattern = '*.R', full.names = T), source)

# Paths
path_taxonomy = '../DATA/taxonomy/IUCN translation list - final.csv'
path_weight_data = '../RESULTS/WEIGHT/clean_dat.RData'
path_weight_results = '../RESULTS/weight/clean_dat.RData'
path_sayol = '../DATA/Sayol et al. 2020/SpeciesTraits.csv'
path_burgio = '../DATA/Burgio et al. 2019/Burgio_et_al_Parrot_Functional_Trait_Data_with_Inferred.csv'
path_mean_weights = '../DATA/cleaned weights litterature/WeightMean.csv'
path_model = '../RESULTS/weight/model.RData'
path_pdf = '../RESULTS/weight/simple weight results on raw data.pdf'
path_out = '../RESULTS/weight/master_dat.RData'

# Load previous results
load(path_weight_results)

# Load taxonomy
taxonomy = taxonomy = read.csv2(path_taxonomy, 
                                na.strings = c('', ' ', 'NA'), stringsAsFactors = F)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Including data from other sources
clean_dat$species = clean_dat$species %>% as.character
## Sayol
sayol = read.csv2(path_sayol, stringsAsFactors = F, na.strings = '')
names(sayol)[names(sayol) == 'Species'] = 'species_original'
sayol$species_original = str_replace(sayol$species_original, '_', ' ')
sayol = taxize.data.frame(sayol, 'species_original', taxonomy, remove_missing = T)
in_sayol = sayol$species[!sayol$species %in% clean_dat$species]
for(sp in in_sayol){
  row_nr = nrow(clean_dat) + 1
  clean_dat[row_nr,] = NA
  clean_dat[row_nr,'species'] = sp
  clean_dat[row_nr,'MeasurementValue'] = sayol$BodySize[sayol$species == sp]
}
## Burgio
burgio = read.csv2(path_burgio, 
                   stringsAsFactors = F, na.strings = '')
names(burgio)[names(burgio) == 'species'] = 'species_original'
burgio = taxize.data.frame(burgio, 'species_original', taxonomy, remove_missing = T)
in_burgio = burgio$species[!burgio$species %in% clean_dat$species]
for(sp in in_burgio){
  row_nr = nrow(clean_dat) + 1
  clean_dat[row_nr,] = NA
  clean_dat[row_nr,'species'] = sp
  clean_dat[row_nr,'MeasurementValue'] = burgio$mass..g.[burgio$species == sp]
}
## Rita - these are cleaned values from the literature
rita = read.csv(path_mean_weights, stringsAsFactors = F, na.strings = '')
names(rita)[names(rita) == 'species'] = 'species_original'
rita$species_original = as.character(rita$species_original)
rita = taxize.data.frame(rita, 'species_original', taxonomy, remove_missing = T)
in_rita = rita$species[!rita$species %in% clean_dat$species]
for(sp in in_rita){
  row_nr = nrow(clean_dat) + 1
  clean_dat[row_nr,] = NA
  clean_dat[row_nr,'species'] = sp
  clean_dat[row_nr,'MeasurementValue'] = rita$varval[rita$species == sp]
}
## Add ID
clean_dat$AnonID[is.na(clean_dat$AnonID)] = 1e6 + (1:length(which(is.na(clean_dat$AnonID))))

# Prepare data
clean_dat$year_month = clean_dat$MeasurementDate %>% str_sub(1, 7)
clean_dat = clean_dat[!duplicated(clean_dat[c('year_month', 'AnonID')]),] # only one sample / id / month
species_trans = 1:length(unique(clean_dat$species))
names(species_trans) = unique(clean_dat$species)
model_dat = list(weight = log(clean_dat$MeasurementValue),
                 species = species_trans[as.character(clean_dat$species)] %>% as.integer,
                 id = clean_dat$AnonID %>% as.factor %>% as.integer)

# Print
print(sprintf('Analysing %s species. This will take a while.', max(model_dat$species)))

# Run model
model = ulam(
  alist(
    weight ~ normal(mu, sigma),
    mu <- a[species] + z_ind[id] * sigma_ind[species],
    a[species] ~ normal(6, 6),
    z_ind[id] ~ dnorm(0, 1),
    sigma_ind[species] ~ dlnorm(sigma_ind_bar, s),
    sigma_ind_bar ~ normal(-2, 0.5),
    s ~ dexp(5),
    sigma ~ dexp(1)
  ), data = model_dat, cores = 6, chains = 6, iter = 6000, warmup = 500,
  control = list(max_treedepth = 15, adapt_delta = 0.99)
)
precis(model)
save(model, file = path_model)

# Test R hat
t = precis(model, depth = 2)
rhat_max = round(t@.Data[[6]], 2) %>% max
print(sprintf('Max rhat is %s.', rhat_max))

# Extract samples and plot results
post = extract.samples(model)
pdf(path_pdf, 25, 10)
plot(model_dat$species,
     model_dat$weight,
     pch = 16, col = alpha('darkgreen', 0.7),
     xlab = 'species index', ylab = 'log body mass [g]')
points(1:max(model_dat$species),
       apply(post$a, 2, mean),
       pch = 16, col = alpha('red', 0.9))
pi = apply(post$a, 2, PI, prob = 0.95)
for(i in 1:max(model_dat$species)) lines(rep(i, 2),
                                         c(pi[1,i],
                                           pi[2,i]),
                                         lwd = 3, col = alpha('red', 0.2))
dev.off()

# Save the averages per species
master_dat = data.frame(species = names(species_trans), 
                        log_mean_body_weight = apply(post$a, 2, mean),
                        log_SE_body_weight = apply(post$a, 2, sd))
save(master_dat, file = path_out)

# Print
print('Analysis done!')
