# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot longevity
# Date started: 15-12-2020
# Date last modified: 15-12-2020
# Author: Simeon Q. Smeele
# Description: Retrieves and summarises variable for the overview of the data. Works with one species at a 
# time. 
# Arguments:
# - species: the species to retrieve the data for
# - data_frame: a list of dataframes to use in order of importance
# - column_name: a vector of column names in those data_frames
# - method: a character, best -> take the first in the list, mean -> take the mean of the values
# - type: what type of data it is
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Function to retrieve variable, takes mean if multiple observations (e.g. subspecies)
retrieve.variable = function(species, data_frame, column_name, method = 'best', type = 'numeric'){
  if(is.data.frame(data_frame)) data_frame = list(data_frame) # in case it's only one data frame
  if(method == 'best' & type == 'numeric'){
    for(i in 1:length(data_frame)){ # the best method
      sub = data_frame[[i]][data_frame[[i]]$species == species,]
      if(nrow(sub) > 0){ # if this data frame has data continue
        y = mean(sub[[column_name[i]]], na.rm = T)
        if(!is.na(y)) break # stop looking if found a value
      } else y = NA
    }
  }
  if(method == 'mean' & type == 'numeric'){ # the mean method
    y = NA
    for(i in 1:length(data_frame)){
      sub = data_frame[[i]][data_frame[[i]]$species == species,]
      if(nrow(sub) > 0){ # if this data frame has data continue
        y = mean(c(sub[[column_name[i]]], y), na.rm = T)
        if(!is.na(y)) break # stop looking if found a value
      }
    }
  }
  if(type == 'categorical'){
    for(i in 1:length(data_frame)){ # the best method
      sub = data_frame[[i]][data_frame[[i]]$species == species,]
      if(nrow(sub) > 0){ # if this data frame has data continue
        y = sub[[column_name[i]]] %>% na.omit %>% mean %>% round
        if(any(!is.na(y))) break # stop looking if found a value
      } else y = NA
    }
  }
  return(y)
}
