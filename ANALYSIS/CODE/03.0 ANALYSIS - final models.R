# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot life expectancy
# Date started: 03-09-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: Analysing the total dataset using Bayesian phylogenetic models. This code needs to be sourced 
# with chdir = T. To better understand the model structure see the mathematical form in the Supplemental
# Methods. 
# This version includes incubation and fledging separately. 
# This version includes clutch size and makes insularity a fixed intercept effect. 
# This version gets rid of old code. 
# This version tests imputation with phylogeny.
# This version uses the full imputation with phylogeny and predictors for all covariates. 
# This version has fixed paths for the new repo. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rethinking', 'taxize', 'ape', 'phytools', 'VennDiagram')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Paths
path_m1 = '../RESULTS/models/m_1_final.RData'
path_m2 = '../RESULTS/models/m_2_final.RData'
path_m3 = '../RESULTS/models/m_3_final.RData'

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# ** Model 1 - cognitive buffer ---- 
clean.data(c('body', 'brain', 'life', 'lat', 'insularity','diet_protein'))
clean_dat$alpha = rep(2, 2)

print('Running model 1, this will take a few days!')

m_1_final <- ulam(
  alist(
    
    # Life expectancy as function of body and brain
    stand_life ~  dnorm(true_life, se_life),
    vector[N_species]: true_life ~ multi_normal(mu_life, SIGMA_life),
    mu_life <- a_life[insularity] +
      b_body_life * true_body[i] +
      b_brain_life * (true_brain[i] - mu_brain) +
      b_lat_life * stand_max_lat +
      b_protein_life * sum(delta_j[1:protein]),
    matrix[N_species, N_species]: SIGMA_life <- cov_GPL2(dmat, etasq_life, rhosq_life, 0.01),
    vector[2]: a_life ~ normal(0, 0.5),
    b_body_life ~ normal(0, 0.5),
    b_brain_life ~ normal(0, 0.5),
    b_lat_life ~ normal(0, 0.5),
    b_protein_life ~ normal(0, 0.5),
    etasq_life ~ dexp(2),
    rhosq_life ~ dexp(0.1),
    vector[3]: delta_j <<- append_row(0 , delta),
    simplex[2]: delta ~ dirichlet(alpha),
    
    # Max lat imputation
    stand_max_lat ~ multi_normal(mu_max_lat, SIGMA_max_lat),
    vector[N_species]: mu_max_lat ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_max_lat <- cov_GPL2(dmat, etasq_max_lat, rhosq_max_lat, 0.01),
    etasq_max_lat ~ dexp(2),
    rhosq_max_lat ~ dexp(0.1),
    
    # Brain as function of body
    stand_brain ~ normal(true_brain, se_brain),
    vector[N_species]: true_brain ~ multi_normal(mu_brain, SIGMA_brain),
    mu_brain <- a_brain + b_body_brain * true_body[i],
    matrix[N_species, N_species]: SIGMA_brain <- cov_GPL2(dmat, etasq_brain, rhosq_brain, 0.01),
    a_brain ~ normal(0, 0.5),
    b_body_brain ~ normal(0, 0.5),
    etasq_brain ~ dexp(2),
    rhosq_brain ~ dexp(0.1),
    
    # Body size
    stand_body ~ normal(true_body, se_body),
    vector[N_species]: true_body ~ multi_normal(mu_body, SIGMA_body),
    vector[N_species]: mu_body ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_body <- cov_GPL2(dmat, etasq_body, rhosq_body, 0.01),
    etasq_body ~ dexp(2),
    rhosq_body ~ dexp(0.1)
    
  ), data = clean_dat, chains = 4, cores = 4,
  warmup = 500, iter = 2000, control = list(max_treedepth = 15))

print('Finished model 1. Thank you for waiting. Here are the results.')

precis(m_1_final, prob = 0.95, 
       pars = c('a_life', 'b_body_life', 'b_brain_life', 'b_lat_life',
                'b_protein_life'), depth = 2) %>% plot %>% print

save(m_1_final, file = path_m1)

# ** Model 2 - expensive brain ---- 

print('Running model 2, this will take a few days!')

clean.data(c('body', 'brain', 'life', 'lat', 'insularity', 'dev_time', 'diet_protein', 'clutch'))
clean_dat$alpha = rep(2, 2)

str(clean_dat)

m_2_final <- ulam(
  alist(
    
    # Life expectancy as function of body and brain
    stand_life ~  dnorm(true_life, se_life),
    vector[N_species]: true_life ~ multi_normal(mu_life, SIGMA_life),
    mu_life <- a_life[insularity] +
      b_body_life * true_body[i] +
      b_brain_life * (true_brain[i] - mu_brain) +
      b_lat_life * stand_max_lat +
      b_dev_life * (stand_inc + stand_fledge) / sd_dev +
      b_clutch_life * stand_clutch +
      b_protein_life * sum(delta_j[1:protein]),
    matrix[N_species, N_species]: SIGMA_life <- cov_GPL2(dmat, etasq_life, rhosq_life, 0.01),
    vector[2]: a_life ~ normal(0, 0.5),
    b_body_life ~ normal(0, 0.5),
    b_brain_life ~ normal(0, 0.5),
    b_lat_life ~ normal(0, 0.5),
    b_dev_life ~ normal(0, 0.5),
    b_clutch_life ~ normal(0, 0.5),
    b_protein_life ~ normal(0, 0.5),
    etasq_life ~ dexp(2),
    rhosq_life ~ dexp(0.1),
    vector[3]: delta_j <<- append_row(0 , delta),
    simplex[2]: delta ~ dirichlet(alpha),
    
    # Max lat imputation
    stand_max_lat ~ multi_normal(mu_max_lat, SIGMA_max_lat),
    vector[N_species]: mu_max_lat ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_max_lat <- cov_GPL2(dmat, etasq_max_lat, rhosq_max_lat, 0.01),
    etasq_max_lat ~ dexp(2),
    rhosq_max_lat ~ dexp(0.1),
    
    # Dev time imputation
    stand_inc ~ multi_normal(mu_inc, SIGMA_inc),
    mu_inc <- a_inc + 
      b_brain_inc * (true_brain[i] - mu_brain) + 
      b_body_inc * true_body[i],
    a_inc ~ normal(0, 0.5),
    b_brain_inc ~ normal(0, 0.5),
    b_body_inc ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_inc <- cov_GPL2(dmat, etasq_inc, rhosq_inc, 0.01),
    etasq_inc ~ dexp(2),
    rhosq_inc ~ dexp(0.1),
    stand_fledge ~ multi_normal(mu_fledge, SIGMA_fledge),
    mu_fledge <- a_fledge + 
      b_brain_fledge * (true_brain[i] - mu_brain) + 
      b_body_fledge * true_body[i] + 
      b_clutch_fledge * stand_fledge,
    a_fledge ~ normal(0, 0.5),
    b_brain_fledge ~ normal(0, 0.5),
    b_body_fledge ~ normal(0, 0.5),
    b_clutch_fledge ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_fledge <- cov_GPL2(dmat, etasq_fledge, rhosq_fledge, 0.01),
    etasq_fledge ~ dexp(2),
    rhosq_fledge ~ dexp(0.1),
    
    # Clutch size imputation
    stand_clutch ~ multi_normal(mu_clutch, SIGMA_clutch),
    mu_clutch <- a_clutch + 
      b_brain_clutch * (true_brain[i] - mu_brain) +
      b_lat_clutch * stand_max_lat,
    a_clutch ~ normal(0, 0.5),
    b_brain_clutch ~ normal(0, 0.5),
    b_body_clutch ~ normal(0, 0.5),
    b_lat_clutch ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_clutch <- cov_GPL2(dmat, etasq_clutch, rhosq_clutch, 0.01),
    etasq_clutch ~ dexp(2),
    rhosq_clutch ~ dexp(0.1),
    
    # Brain as function of body
    stand_brain ~ normal(true_brain, se_brain),
    vector[N_species]: true_brain ~ multi_normal(mu_brain, SIGMA_brain),
    mu_brain <- a_brain + b_body_brain * true_body[i],
    matrix[N_species, N_species]: SIGMA_brain <- cov_GPL2(dmat, etasq_brain, rhosq_brain, 0.01),
    a_brain ~ normal(0, 0.5),
    b_body_brain ~ normal(0, 0.5),
    etasq_brain ~ dexp(2),
    rhosq_brain ~ dexp(0.1),
    
    # Body size
    stand_body ~ normal(true_body, se_body),
    vector[N_species]: true_body ~ multi_normal(mu_body, SIGMA_body),
    vector[N_species]: mu_body ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_body <- cov_GPL2(dmat, etasq_body, rhosq_body, 0.01),
    etasq_body ~ dexp(2),
    rhosq_body ~ dexp(0.1)
    
  ), data = clean_dat, chains = 4, cores = 4,
  warmup = 500, iter = 2000, control = list(max_treedepth = 15))

print('Finished model 2. Thank you for waiting. Here are the results.')

precis(m_2_final, prob = 0.95, 
       pars = c('a_life', 'b_body_life', 'b_brain_life', 'b_lat_life', 'b_dev_life',
                'b_protein_life', 'b_clutch_life', 'a_brain', 'b_body_brain'), depth = 2) %>% plot

save(m_2_final, file = path_m2)

# ** Model 3 - expensive brain + AFR ---- 

print('Running model 3, this will take a while!')

clean.data(c('body', 'brain', 'life', 'lat', 'insularity', 'dev_time', 'diet_protein', 'clutch'))
clean_dat$alpha = rep(2, 2)

afr = read.csv2('../RESULTS/AFR/5th percentile results.csv')
log_afr = log(afr$afr_5th_percentile)
sd_afr = sd(log_afr)
mean_afr = mean(log_afr)
stand_afr = (log_afr - mean_afr) / sd_afr
clean_dat$stand_afr = sapply(dat$species, function(sp){
  y = stand_afr[afr$species == sp]
  if(length(y) == 0) y = NA
  return(y)
}) %>% as.numeric

for(name in names(clean_dat)[c(2:10, 12:14)])
  clean_dat[name][[1]] = clean_dat[name][[1]][!is.na(clean_dat$stand_afr)]
clean_dat$dmat = clean_dat$dmat[!is.na(clean_dat$stand_afr), !is.na(clean_dat$stand_afr)]
clean_dat$stand_afr = clean_dat$stand_afr[!is.na(clean_dat$stand_afr)]
clean_dat$N_species = length(clean_dat$stand_life)

m_3_final <- ulam(
  alist(
    
    # Life expectancy as function of body and brain
    stand_life ~  dnorm(true_life, se_life),
    vector[N_species]: true_life ~ multi_normal(mu_life, SIGMA_life),
    mu_life <- a_life[insularity] +
      b_body_life * true_body[i] +
      b_brain_life * (true_brain[i] - mu_brain) +
      b_lat_life * stand_max_lat +
      b_dev_life * (stand_inc + stand_fledge) / sd_dev +
      b_clutch_life * stand_clutch +
      b_protein_life * sum(delta_j[1:protein]) +
      b_afr_life * stand_afr,
    matrix[N_species, N_species]: SIGMA_life <- cov_GPL2(dmat, etasq_life, rhosq_life, 0.01),
    vector[2]: a_life ~ normal(0, 0.5),
    b_body_life ~ normal(0, 0.5),
    b_brain_life ~ normal(0, 0.5),
    b_lat_life ~ normal(0, 0.5),
    b_dev_life ~ normal(0, 0.5),
    b_clutch_life ~ normal(0, 0.5),
    b_protein_life ~ normal(0, 0.5),
    b_afr_life ~ normal(0, 0.5),
    etasq_life ~ dexp(2),
    rhosq_life ~ dexp(0.1),
    vector[3]: delta_j <<- append_row(0 , delta),
    simplex[2]: delta ~ dirichlet(alpha),
    
    # Max lat imputation
    stand_max_lat ~ multi_normal(mu_max_lat, SIGMA_max_lat),
    vector[N_species]: mu_max_lat ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_max_lat <- cov_GPL2(dmat, etasq_max_lat, rhosq_max_lat, 0.01),
    etasq_max_lat ~ dexp(2),
    rhosq_max_lat ~ dexp(0.1),
    
    # Dev time imputation
    stand_inc ~ multi_normal(mu_inc, SIGMA_inc),
    mu_inc <- a_inc + 
      b_brain_inc * (true_brain[i] - mu_brain) + 
      b_body_inc * true_body[i],
    a_inc ~ normal(0, 0.5),
    b_brain_inc ~ normal(0, 0.5),
    b_body_inc ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_inc <- cov_GPL2(dmat, etasq_inc, rhosq_inc, 0.01),
    etasq_inc ~ dexp(2),
    rhosq_inc ~ dexp(0.1),
    stand_fledge ~ multi_normal(mu_fledge, SIGMA_fledge),
    mu_fledge <- a_fledge + 
      b_brain_fledge * (true_brain[i] - mu_brain) + 
      b_body_fledge * true_body[i] + 
      b_clutch_fledge * stand_fledge,
    a_fledge ~ normal(0, 0.5),
    b_brain_fledge ~ normal(0, 0.5),
    b_body_fledge ~ normal(0, 0.5),
    b_clutch_fledge ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_fledge <- cov_GPL2(dmat, etasq_fledge, rhosq_fledge, 0.01),
    etasq_fledge ~ dexp(2),
    rhosq_fledge ~ dexp(0.1),
    
    # Clutch size imputation
    stand_clutch ~ multi_normal(mu_clutch, SIGMA_clutch),
    mu_clutch <- a_clutch + 
      b_brain_clutch * (true_brain[i] - mu_brain) +
      b_lat_clutch * stand_max_lat,
    a_clutch ~ normal(0, 0.5),
    b_brain_clutch ~ normal(0, 0.5),
    b_body_clutch ~ normal(0, 0.5),
    b_lat_clutch ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_clutch <- cov_GPL2(dmat, etasq_clutch, rhosq_clutch, 0.01),
    etasq_clutch ~ dexp(2),
    rhosq_clutch ~ dexp(0.1),
    
    # Brain as function of body
    stand_brain ~ normal(true_brain, se_brain),
    vector[N_species]: true_brain ~ multi_normal(mu_brain, SIGMA_brain),
    mu_brain <- a_brain + b_body_brain * true_body[i],
    matrix[N_species, N_species]: SIGMA_brain <- cov_GPL2(dmat, etasq_brain, rhosq_brain, 0.01),
    a_brain ~ normal(0, 0.5),
    b_body_brain ~ normal(0, 0.5),
    etasq_brain ~ dexp(2),
    rhosq_brain ~ dexp(0.1),
    
    # Body size
    stand_body ~ normal(true_body, se_body),
    vector[N_species]: true_body ~ multi_normal(mu_body, SIGMA_body),
    vector[N_species]: mu_body ~ normal(0, 0.5),
    matrix[N_species, N_species]: SIGMA_body <- cov_GPL2(dmat, etasq_body, rhosq_body, 0.01),
    etasq_body ~ dexp(2),
    rhosq_body ~ dexp(0.1)
    
  ), data = clean_dat, chains = 4, cores = 4,
  warmup = 500, iter = 2000, control = list(max_treedepth = 15))

print('Finished model 3. Thank you for waiting. Here are the results.')

precis(m_3_final, prob = 0.95, 
       pars = c('a_life', 'b_body_life', 'b_brain_life', 'b_lat_life', 'b_dev_life', 'b_afr_life', 
                'b_protein_life', 'b_clutch_life', 'a_brain', 'b_body_brain'), depth = 2) %>% print

save(m_3_final, file = path_m3)
