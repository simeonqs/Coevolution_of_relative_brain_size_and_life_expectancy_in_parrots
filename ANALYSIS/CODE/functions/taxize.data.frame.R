# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot longevity
# Date started: 15-12-2020
# Date last modified: 28-06-2021
# Author: Simeon Q. Smeele
# Description: Standardises taxonomy based on provided synonym data frame. Returns the updated data frame. 
# The 'correct' species name are in a column named 'species'. If your original data frame contains such a 
# column it will be replaced. 
# Arguments: 
# - data_frame: the data frame to be taxized
# - column_name: the column name of the column with the original names
# - taxonomy: the synonym data frame with original_species and species as two columns
# - printer: if set to TRUE, prints the species names that are not found in the list
# - remove_missing: whether or not to remove missing species
# - incl_sub: logical, whether or not to include subspecies - to be split into species
# - sub_taxonomy: the data frame containing the sub species taxonomy
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

taxize.data.frame = function(data_frame, column_name, taxonomy, printer = F, remove_missing = T,
                             incl_sub = F, sub_taxonomy = NULL){
  
  # Taxize at subspecies level
  if(incl_sub){
    data_frame$species = sapply(data_frame[[column_name]], function(x){
      tax_spec = sub_taxonomy$species[sub_taxonomy$original_species == x]
      ifelse(length(tax_spec) == 0, x, tax_spec)
    })
  }
  
  # Taxize at species level
  data_frame$species = sapply(data_frame[[column_name]], function(x){
    split = strsplit(x, ' ')[[1]]
    x = paste(split[1], split[2])
    tax_spec = taxonomy$species[taxonomy$original_species == x]
    ifelse(length(tax_spec) == 0, NA, tax_spec)
  })
  
  # Print which species are not found
  if(printer & any(is.na(data_frame$species))){
    print('Missing these species:')
    print(data_frame[[column_name]][is.na(data_frame$species)])
  }
  
  # Remove missing species
  if(remove_missing) data_frame = data_frame[!is.na(data_frame$species),]
  
  # Return result
  return(data_frame)
  
} # End taxize.data.frame




