# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot longevity
# Date started: 16-12-2020
# Date last modified: 02-04-2021
# Author: Simeon Q. Smeele
# Description: Standardises the data for the Bayesian models. 
# Arguments:
# - to_run: a character vectors indicating what data to include
# This version only contains variables for the first paper and requires '02.1 DATA - merge all.R' to be 
# sourced first. 
# This version cleans incubation and fledging separately. 
# This version also includes clutch size. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

clean.data = function(to_run = NULL){
  
  # Start with number of species
  clean_dat = list(N_species = nrow(dat))
  
  # Life expectancy
  if('life' %in% to_run){
    log_life = dat$log_mean_life_exp
    sd_life = sd(log_life, na.rm = T)
    mean_life = mean(log_life, na.rm = T)
    clean_dat$stand_life = as.numeric((log_life - mean_life) / sd_life)
    se_life = dat$log_SE_life_exp
    clean_dat$se_life = as.numeric(se_life / sd_life) # bit weird, but this is actually the scaled SE now
    ## for now if SE is unknown, replace by 0.01, since it's mean is imputed anyway
    clean_dat$se_life = ifelse(is.na(clean_dat$se_life), 0.01, clean_dat$se_life)
  }
  
  # Body weight
  if('body' %in% to_run){
    log_body = dat$log_mean_body_weight
    sd_body = sd(log_body, na.rm = T)
    mean_body = mean(log_body, na.rm = T)
    clean_dat$stand_body = as.numeric((log_body - mean_body) / sd_body)
    se_body = dat$log_SE_body_weight
    clean_dat$se_body = as.numeric(se_body / sd_body) # bit weird, but this is actually the scaled SE now
    ## for now if SE is unknown, replace by 0.01, since it's mean is imputed anyway
    clean_dat$se_body = ifelse(is.na(clean_dat$se_body), 0.01, clean_dat$se_body)
  }
  
  # Brain weight
  if('brain' %in% to_run){
    log_brain = dat$log_mean_brain_size
    sd_brain = sd(log_brain, na.rm = T)
    mean_brain = mean(log_brain, na.rm = T)
    clean_dat$stand_brain = as.numeric((log_brain - mean_brain) / sd_brain)
    se_brain = dat$log_SE_brain_size
    clean_dat$se_brain = as.numeric(se_brain / sd_brain) # bit weird, but this is actually the scaled se now
    ## for now if se is unknown, replace by 0.01, since it's mean is imputed anyway
    clean_dat$se_brain = ifelse(is.na(clean_dat$se_brain), 0.01, clean_dat$se_brain)
  }
  
  # Insularity
  ## make into index: 1 = continental, 2 = insular
  if('insularity' %in% to_run) clean_dat$insularity = dat$insularity + 1
  
  # Developmental time
  if('dev_time' %in% to_run){
    log_inc = log(dat$incubation_days)
    log_fledge = log(dat$fledging_age_days)
    mean_inc = mean(log_inc, na.rm = T)
    mean_fledge = mean(log_fledge, na.rm = T)
    sd_inc = sd(log_inc, na.rm = T)
    sd_fledge = sd(log_fledge, na.rm = T)
    clean_dat$stand_inc = as.numeric((log_inc - mean_inc) / sd_inc)
    clean_dat$stand_fledge = as.numeric((log_fledge - mean_fledge) / sd_fledge)
    clean_dat$sd_dev = sd(clean_dat$stand_inc + clean_dat$stand_fledge, na.rm = T)
  }
  
  # Clutch size
  if('clutch' %in% to_run){
    log_clutch = log(dat$clutch_size_n)
    mean_clutch = mean(log_clutch, na.rm = T)
    sd_clutch = sd(log_clutch, na.rm = T)
    clean_dat$stand_clutch = as.numeric((log_clutch - mean_clutch) / sd_clutch)
  }
  
  # Diet protein
  if('diet_protein' %in% to_run){
    clean_dat$protein = dat$diet_protein
  }
  
  # Maximum latitude range
  if('lat' %in% to_run){
    # Max lat
    max_lat = dat$max_lat
    mean_max_lat = mean(max_lat, na.rm = T)
    sd_max_lat = sd(max_lat, na.rm = T)
    clean_dat$stand_max_lat = as.numeric((max_lat - mean_max_lat) / sd_max_lat)
  }
  
  # Phylogenetic distance matrix
  dmat = cophenetic(tree)
  clean_dat$dmat = dmat / max(dmat)
  attr(clean_dat$dmat, 'dimnames') = NULL
  
  # Return
  clean_dat <<- clean_dat
  
} # End clean.dat
