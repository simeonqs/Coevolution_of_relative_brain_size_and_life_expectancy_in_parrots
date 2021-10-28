# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: chapter IV
# Date started: 06-01-2021
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script takes max latitude from the BotW shapefile and exports it per species in 
# master_dat.RData. Needs to be sourced with chdir = T.
# NOTE: This part cannot be reproduced since BOTW does not allow data to be shared directly. You can, however,
# download the data yourself and and save it under DATA/BOTW. Exact reproduction will still not be possible,
# but the results should be very similar. 
# This version was moved to the new paper repo and paths were updated. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# DATA ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Loading libraries
libraries = c('data.table', 'tidyverse', 'rgdal', 'sf')
for(i in libraries){
  if(! i %in% installed.packages()) lapply(i, install.packages)
  lapply(libraries, require, character.only = TRUE)
}

# Load functions
.functions = sapply(list.files('functions', pattern = '*.R', full.names = T), source)

# Path
path_taxize = '../DATA/taxonomy/IUCN translation list - final.csv'
path_botw = '../DATA/BOTW/BOTW.gdb'
path_pdf = '../RESULTS/latitude/maps.pdf'
path_out = '../RESULTS/latitude/master_dat.RData'

# Read data - takes very long!
dat = read_sf(path_botw) 
taxonomy = read.csv2(path_taxize, 
                     na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
world_map = map_data('world')

# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# ANALYSIS ----
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Taxize the names - should all match since it's IUCN taxonomy
out = data.frame(index = 1:length(dat$binomial), original_species = dat$binomial)
out = out[dat$origin == 1,] # only keep native range (I think)
out = taxize.data.frame(out, 'original_species', taxonomy)

# Run trough species and get min and max latitude
out$min_lat = NA
out$max_lat = NA
pdf(path_pdf)
for(i in 1:nrow(out)){
  sub = dat$Shape[[out$index[i]]] %>% lapply(function(x) as.data.frame(x[[1]])) %>% bind_rows
  out$min_lat[i] = min(sub$V2)
  out$max_lat[i] = max(sub$V2)
  print(
    ggplot() +
      # The map
      geom_polygon(data = world_map, fill= 'white', aes(x=long, y = lat, group = group)) +
      coord_fixed(1.3) +
      geom_tile() +
      labs(x = 'Longitude', y = 'Latitude', title = dat$binomial[out$index[i]]) +
      # The species
      geom_point(data = sub, aes(x = V1, y = V2), col = 'darkgreen') +
      scale_color_brewer() +
      theme(panel.background = element_rect(fill = '#9EC7F3',
                                            colour = '#9EC7F3',
                                            size = 0.5, linetype = 'solid'))
    
  )
} 
dev.off()

# Get the max distance from equator
## there might be multiple rows per species, so this takes the min/max of all the rows
## for that species
master_dat = data.frame(species = na.omit(unique(taxonomy$species)))
master_dat$max_lat = sapply(master_dat$species, function(x){
  min = out$min_lat[out$species == x]
  max = out$max_lat[out$species == x]
  if(length(min) == 0) {min = NA; max = NA}
  return(max(abs(c(min, max))))
})

# Remove migratory species *** NOT COMPLETE ***
master_dat$max_lat[master_dat$species %in% 
                     c('Lathamus discolor', 'Neophema chrysogaster', 
                       'Cyanoliseus patagonus', 'Rhynchopsitta pachyrhyncha')] = NA

# Write the output
save(master_dat, file = path_out)

# Print
print('Thank you for waiting. Processed shapefile and saved max latitude.')
