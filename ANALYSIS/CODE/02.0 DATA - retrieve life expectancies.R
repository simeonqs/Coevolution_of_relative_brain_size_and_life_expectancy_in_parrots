# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: chapter IV
# Date started: 03-03-2021
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script loads all accepted species names from the synonym list and retrieves and 
# summarises the life expectancy data. It outputs an RData file with a data frame containing the species 
# names and the log life expectancies + log SE (master_dat). 
# NOTE: This scripted should be sourced with chdir = T. 
# NOTE: For this you need all the model outputs, which do not fit on the repo. 
# This version uses check table and uses the models without sex as covariate. 
# This version only uses the GO models. 
# This version was moved to the new paper repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'BaSTA.ZIMS', 'parallel', 'snowfall', 'rethinking')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Path
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_life_exp_results = '../RESULTS/life expectancy/results no sex'
path_life_exp_results_reruns = '../RESULTS/life expectancy/results no sex - reruns'
path_check = '../RESULTS/life expectancy/checklist_2021-03-23 10:57:36 results no sex.csv'
path_fernando = 'functions/functions Fernando.R'
path_out = '../RESULTS/life expectancy/master_dat.RData'

# Source functions Fernando
source(path_fernando)

# Load data
taxonomy = taxonomy = read.csv2(path_taxize, na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
check_table = read.csv2(path_check)

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Creating list with paths to all bathtub model outputs
files = list.files(path_life_exp_results, '*.RData', full.names = T)
species = check_table$species[check_table$keep_GO == 1]
species_rdata = files %>% str_remove(paste0(path_life_exp_results, '/')) %>% str_remove('.RData')

# Subset for species where I didn't remove pdf
files = files[species_rdata %in% species]
species_files = files %>% str_remove(paste0(path_life_exp_results, '/')) %>% str_remove('.RData')

# Retrieve life expectancy and SE
dat = data.frame()
for(i in 1:length(files)){
  
  # Load file
  load(files[i])
  
  # Get best model
  sub_check_table = check_table[check_table$species == species_files[i],]
  md = 'GObt' # always take GObt
  conv2 = ifelse(outBasta$GObt$convmessage == "All parameters converged properly.\n", 1, 0)
  ## Take the rerun if GO did not converge
  if(conv2 == 0) load(files[i] %>% str_replace('results no sex', 'results no sex - reruns'))
  
  # Calculate life expectancy
  model <- "GO"
  shape <- "bathtub"
  mdsh <-  "GObt"
  out <- outBasta[[md]]
  
  ## Mortality function:
  mort <- function(theta, ...) UseMethod("mort")
  mort.matrix <- .DefineMortMatrix(model = model, shape = shape)
  mort.numeric <- .DefineMortNumeric(model = model, shape = shape)
  
  ## Cummulative hazard:
  cumHaz <- function(theta, ...) UseMethod("cumHaz")
  cumHaz.matrix <- .DefineCumHazMatrix(model = model, shape = shape)
  cumHaz.numeric <- .DefineCumHazNumeric(model = model, shape = shape)
  
  ## Survival:
  surv <- function(theta, x) {
    exp(-cumHaz(theta = theta, x = x))
  }
  
  ## Life expectancy:
  lifeexp <- function(Sx, dx) {
    sum(Sx * dx) / Sx[1]
  }
  
  ## Function to name mortality parameters:
  NameTheta <- function(model = "GO", shape = "simple") {
    if (model == "EX") {
      nameTh <- "b"
    } else if (model %in% c("GO", "WE")) {
      nameTh <- c("b0", "b1")
    } else {
      nameTh <- c("b0", "b1", "b2")
    }
    if (shape == "Makeham") {
      nameTh <- c("c", nameTh)
    } else if (shape == "bathtub") {
      nameTh <- c("a0", "a1", "c", nameTh)
    }
    return(nameTh)
  }
  
  ## Function to find max age based on survival level:
  FindMaxX <- function(theta, lev = 0.0001, errlev = 0.00001) {
    xseq <- c(0, 10^(0:5))
    Sseq <- surv(theta, xseq)
    ilev <- which(Sseq < lev)[1]
    err <- 1
    while(err > errlev) {
      xseq <- seq(xseq[ilev -1], xseq[ilev], length = 100)
      Sseq <- surv(theta, xseq)
      ilev <- which(Sseq < lev)[1]
      err <- abs(Sseq[ilev] - lev)
    }
    xmax <- xseq[ilev]
    return(xmax)
  }
  
  ## Function to calculate life expectancies in parallel
  CalcExParall <- function(sim) {
    # index of rows to include:
    thInts <- floor(seq(1, nth, nth / nsims))
    if (!nth %in% thInts) thInts <- c(thInts, nth)
    
    # species vector for simion:
    if (sim == 1) {
      idthrun <- thInts[1]:thInts[2]
    } else {
      idthrun <- (thInts[sim] + 1):thInts[sim + 1]
    }
    nidthrun <- length(idthrun)
    
    # life expectancy by sex matrix:
    exmat <- matrix(NA, nidthrun, 1, dimnames = list(NULL, c("NoSex")))
    
    # Find mean theta for the species:
    theta <- out$coefficients[, 1]
    names(theta) <- NameTheta(model = model, shape = shape)
    
    # Find maximum x value for integration:
    maxx <- FindMaxX(theta)
    
    # vector of ages:
    xv <- seq(0, maxx, dx)
    
    # Length of xv vector:
    n <- length(xv)
    
    # Extract theta:
    thmat <- out$theta[idthrun, ]
    if (is.matrix(thmat)) {
      colnames(thmat) <- NameTheta(model = model, shape = shape)
    }
    
    # Calculate vector of life expectancies:
    if (model == "EX") {
      exsx <- 1 / thmat
    } else {
      exsx <- apply(thmat, 1, function(th) {
        Sx <- surv(th, xv)
        e0 <- lifeexp(Sx, dx)
        return(e0)
      })
    }
    exmat <- exsx
  }
  
  ## Number of theta vectors:
  nth <- nrow(out$theta)
  
  ## Age increments for integral:
  dx <- 0.001
  
  ## Number of cpus to run in parallel (CHANGE AS NEEDED)
  ncpus <- 4
  
  ## Number of sims is equal to ncpus:
  nsims <- ncpus
  
  ## Variables to export to cpus:
  cpuVars <- c("out", "nth", "dx", "FindMaxX", "nsims", "surv", "cumHaz",
               "cumHaz.numeric", "cumHaz.matrix", "lifeexp", "NameTheta",
               "model", "shape")
  
  ## Run analysis in paralllel:
  sfInit(parallel = TRUE, cpus = ncpus)
  sfExport(list = cpuVars)
  Start <- Sys.time()
  outTabList <- sfClusterApplyLB(1:ncpus, CalcExParall)
  End <- Sys.time()
  sfStop()
  
  ## Extract results from parallel runs:
  exmat <- unlist(outTabList)
  
  # Plot density
  dens(exmat, main = species[i])
  
  # Saving
  dat = bind_rows(dat, data.frame(
    species = species[i],
    mean_log = mean(log(exmat)),
    se_log = sd(log(exmat))))
  
} # End i loop (files)

# Get full species list and add relevant data with recognisable names
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat$log_mean_life_exp = sapply(master_dat$species, function(sp){
  y = dat$mean_log[dat$species == sp]
  return(ifelse(length(y) == 0, NA, y))
})
master_dat$log_SE_life_exp = sapply(master_dat$species, function(sp){
  y = dat$se_log[dat$species == sp]
  return(ifelse(length(y) == 0, NA, y))
})

# Save 
save(master_dat, file = path_out)

# Print
print(sprintf('Saved life expectancy for %s species, with averages of %s posterior samples per species.',
              nrow(dat), length(exmat)))
print(sprintf('Log life expectancy ranges between %s and %s, log SE ranges between %s and %s.',
              round(min(master_dat$log_mean_life_exp, na.rm = T), 3), 
              round(max(master_dat$log_mean_life_exp, na.rm = T), 3),
              round(min(master_dat$log_SE_life_exp, na.rm = T), 3), 
              round(max(master_dat$log_SE_life_exp, na.rm = T), 3)))
