# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>
# Project: parrot longevity
# Date started: 17-12-2020
# Date last modified: 23-08-2021
# Author: Simeon Q. Smeele
# Description: This script takes all the master_dat.RData files and merges them into one dataframe (dat) 
# which contains all the variables. Variables can then be standardised for the models with clean.data(). 
# It also loads the phylogenetic tree. Should be sourced using chdir = T. 
# This version also includes clutch size. 
# This version has fixed paths for the new repo. 
# >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>

# Load functions
.functions = sapply(list.files('functions', pattern = '*.R', full.names = T), source)

# Load data and merge
taxonomy = read.csv2('../DATA/taxonomy/IUCN translation list - final.csv', 
                     na.strings = c('', ' ', 'NA'), stringsAsFactors = F)
dat = data.frame(species = na.omit(unique(taxonomy$species))) # start with all the correct species names
dat = data.frame(species = dat[dat$species != 'Strigops habroptila',]) # removing kakapo because it's such 
                                                                       # an outlier
load('../RESULTS/weight/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/brain/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/latitude/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/developmental time/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/life expectancy/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/diet/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/insularity/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)
load('../RESULTS/clutch size/master_dat.RData')
dat = merge(dat, master_dat, by = 'species', all.x = T, all.y = F)

# Remove extinct species
dat = dat[!dat$species %in% c('Psittacula wardi', 'Lophopsittacus mauritianus', 'Mascarinus mascarin',
                              'Eupsittula nana', 'Ara tricolor', 'Psittacula exsul'),]

# Load tree
tree = read.tree('../DATA/Burgio et al. 2019/5.ParrotSupertree.tre')

# Tree tips
tree.tips = tree$tip.label
tree.tips.or = tree.tips
tree.tips = str_replace(tree.tips.or, "_", " ")

# Taxize tree
tree$tip.label = sapply(tree.tips, function(x){
  if(x %in% taxonomy$original_species) 
    str_replace(taxonomy$species[taxonomy$original_species == x], " ", "_") else 
    tree$tip.label[tree.tips == x]
}) %>% as.character
tree.tips = tree$tip.label
tree.tips.or = tree.tips
tree.tips = str_replace(tree.tips.or, "_", " ")

# Missing in tree
test = unique(dat$species[!dat$species %in% tree.tips])
test[test %in% taxonomy$original_species %>% `!`]

# Pruning tree
drop.tips = ifelse(tree.tips %in% dat$species, NA, tree.tips.or)
drop.tips = na.omit(drop.tips)
tree = drop.tip(tree, drop.tips)
tree.tips.or = tree$tip.label
tree.tips = str_replace(tree.tips.or, "_", " ")

# Dropping duplicated tips
tree = drop.tip(tree, tree.tips.or[duplicated(tree.tips)])
tree.tips.or = tree$tip.label
tree.tips = str_replace(tree.tips.or, "_", " ")

# Remove species that are not in tree
dat = dat[dat$species %in% tree.tips,]

# Organise data in same order as tips
rownames(dat) = dat$species
dat = dat[tree.tips,]

# Plot correlation plot
# pdf('../RESULTS/correlations.pdf', 10, 10)
# pairs(dat[,-1])
# dev.off()

# Print
print('Loaded all data, which is available in your environment as `dat`.')