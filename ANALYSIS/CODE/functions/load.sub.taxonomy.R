# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: chapter IV
# Date started: 28-06-2021
# Date last modified: 28-06-2021
# Author: Simeon Q. Smeele
# Description: Loads and cleans Simon's sub-species taxonomy. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

load.sub.taxonomy = function(path){

  sub_taxonomy_orig = read.csv2(path) # read original file
  sub_taxonomy = data.frame(original_species = character(), species = character()) # prepare empty data.frame
  for(i in 1:nrow(sub_taxonomy_orig)){ # run through rows in original file
    original_species = str_split(sub_taxonomy_orig$original_species[i], ' ')[[1]] # different levels
    original_species = original_species[nchar(original_species) > 1] # remove genus-level
    new_species = c()
    for(j in seq(1, length(original_species) - 1, by = 3)){ # collapse into sub-species level
      new_species = c(new_species, paste(original_species[j:(j+2)], collapse = ' '))
    }
    sub_taxonomy = rbind(sub_taxonomy, # save into new data.frame
                         data.frame(
                           original_species = new_species,
                           species = sub_taxonomy_orig$species[i]
                         ))
  }
  
  return(sub_taxonomy)
  
} # End load.sub.taxonomy
