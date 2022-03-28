# Coevolution of relative brain size and life expectancy in parrots

The R code needed to replicate results from the article:

```
Smeele SQ et al. 2022 Coevolution of relative brain size and life expectancy in parrots. Proc. R. Soc. B 289: 20212397. https://doi.org/10.1098/rspb.2021.2397
```

------------------------------------------------

**Abstract**

Previous studies have demonstrated a correlation between longevity and brain size in a variety of taxa. Little research has been devoted to understanding this link in parrots; yet parrots are well-known for both their exceptionally long lives and cognitive complexity. We employed a large-scale comparative analysis that investigated the influence of brain size and life history variables on longevity in parrots. Specifically, we addressed two hypotheses for evolutionary drivers of longevity: the Cognitive Buffer Hypothesis, which proposes that increased cognitive abilities enable longer life spans, and the Expensive Brain Hypothesis, which holds that increases in life span are caused by prolonged developmental time of, and increased parental investment in, large brained offspring. We estimated life expectancy from detailed zoo records for 133,818 individuals across 244 parrot species. Using a principled Bayesian approach that addresses data uncertainty and imputation of missing values, we found a consistent correlation between relative brain size and life expectancy in parrots. This correlation was best explained by a direct effect of relative brain size. Notably, we found no effects of developmental time, clutch size, or age at first reproduction. Our results suggest that selection for enhanced cognitive abilities in parrots have in turn promoted longer lifespans. 


------------------------------------------------

**The folders contain:**

ANALYSIS:
  - CODE: the code to replicate results
  - DATA: raw data; only non-sensitive data is available
  - RESULTS: intermediate results and figures; only small files can be made available

NOTE: Some data contains sensitive information (zoo records) and can therefore not be made available. Processed data will be made available.

------------------------------------------------

**File information:**

NOTE: each code file contains additional information about author, date modified and description. 

- README.md: overview of repo and all files
- .gitignore: which files not to sync to GitHub
- Coevolution_of_relative_brain_size_and_life_expectancy_in_parrots.Rproj: R Studio Project file; if you open the code from this file all paths are relative to the main folder

- ANALYSIS/CODE/README.md: overview of the code pipepline
- ANALYSIS/CODE/00.0 CLEANING - body weight.R: code to clean the body weight data from ZIMS; data to run the code is sensitive and cannot be shared
- ANALYSIS/CODE/00.0 CLEANING - life expectancy.R: code to clean the life expectancy data from ZIMS; data to run the code is sensitive and cannot be shared
- ANALYSIS/CODE/00.1 PRE-ANALYSIS - life expectancy.R: code to run the life expectancy models; data to run the code is sensitive and cannot be shared
- ANALYSIS/CODE/01.0 PRE-ANALYSIS - AFR.R: code to analyse the age of first possible reproduction data; data to run the code is sensitive and cannot be shared
- ANALYSIS/CODE/01.0 PRE-ANALYSIS - body weight.R: code to run the body weight models; data to run the code is sensitive and cannot be shared
- ANALYSIS/CODE/01.0 PRE-ANALYSIS - brain size.R: code to run the brain size models
- ANALYSIS/CODE/02.0 DATA - clutch size.R: code to clean the clutch size data from the literature
- ANALYSIS/CODE/02.0 DATA - developmental time.R: code to clean the developmental data from the literature
- ANALYSIS/CODE/02.0 DATA - diet.R: code to clean the diet data from the literature and experts
- ANALYSIS/CODE/02.0 DATA - insularity.R: code to clean the insularity data from the literature
- ANALYSIS/CODE/02.0 DATA - latitude.R: code to clean the latitude data from Birds of the World
- ANALYSIS/CODE/02.0 DATA - retrieve life expectancies.R: code to summarise the life expectancy results; model results contain sensitive information and are too large to be shared
- ANALYSIS/CODE/02.1 DATA - merge all.R: code to merge all data into a single data frame
- ANALYSIS/CODE/03.0 ANALYSIS - final models.R: code to run the final three models
- ANALYSIS/CODE/functions/SubsetNewcore.Simeon.R: function to subset raw data from ZIMS; adapted from code written by Fernando Colchero
- ANALYSIS/CODE/functions/clean.data.R: function to prepare data for the final Bayesian models
- ANALYSIS/CODE/functions/functions Fernando.R: functions written by Fernando Colchero to retrieve life expectancy estimates from the model outputs
- ANALYSIS/CODE/functions/load.sub.taxonomy.R: function to load and clean the sub-species taxonomy
- ANALYSIS/CODE/functions/retrieve.variable.R: function to retrieve a variable from a set of data bases
- ANALYSIS/CODE/functions/taxize.data.frame.R: function to standardise taxonomy of a data set
- ANALYSIS/CODE/markdowns/README.md: short overview
- ANALYSIS/CODE/markdowns/bibliography.bib: Bibtex file with the references for the RMarkdown files
- ANALYSIS/CODE/markdowns/results.Rmd: the Rmarkdown file to compile all final results
- ANALYSIS/CODE/markdowns/results.pdf: pdf with final results
- ANALYSIS/CODE/markdowns/supplemental methods and results.Rmd: RMarkdown file to compile supplemental materials
- ANALYSIS/CODE/markdowns/supplemental methods and results.pdf: pdf with the supplemental materials

- ANALYSIS/DATA/README.md: overview of all data that cannot be shared and links where to access original data if applicable
- ANALYSIS/DATA/ALHD/REAMDE.md: where to download the data
- ANALYSIS/DATA/ALHD/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/Andrew Iwaniuk/data.csv: brain size data from Dr. Andrew Iwaniuk
- ANALYSIS/DATA/Burgio et al. 2019/README.md: where to download the data
- ANALYSIS/DATA/Burgio et al. 2019/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/DSKI/Data/README.md: where to download the data
- ANALYSIS/DATA/DSKI/Data/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/Ducatez et al. 2020/README.md: where to download the data
- ANALYSIS/DATA/Ducatez et al. 2020/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/Ksepka et al. 2020/README.md: where to download the data
- ANALYSIS/DATA/Ksepka et al. 2020/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/Sayol et al. 2020/README.md: where to download the data
- ANALYSIS/DATA/Sayol et al. 2020/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/Schuck-Paim et al. 2008/README.md: where to download the data
- ANALYSIS/DATA/Schuck-Paim et al. 2008/.gitignore: file to make GitHub ignore the data in this folder
- ANALYSIS/DATA/cleaned weights/WeightMean.csv: file with weight cleaned by Rita da Silva for previous publication
- ANALYSIS/DATA/diet/diet data.xlsx: file with diet data from experts
- ANALYSIS/DATA/other litterature/data.csv: file with data from other sources (books)
- ANALYSIS/DATA/taxonomy/README.md: overview
- ANALYSIS/DATA/taxonomy/IUCN sub species taxonomy.csv: sub-species taxonomy used in this study
- ANALYSIS/DATA/taxonomy/IUCN translation list - final.csv: species taxonomy used in this study

- ANALYSIS/RESULTS/README.md: overview
- ANALYSIS/RESULTS/AFR/.gitignore: file to make GitHub ignore a large PDF in this folder
- ANALYSIS/RESULTS/AFR/5th percentile results.csv: the result for the age of first possible reproduction in captivity
- ANALYSIS/RESULTS/brain/.gitignore: file to make GitHub ignore the large model output file
- ANALYSIS/RESULTS/brain/compare mean to raw data.pdf: pdf file comparing the model means to the raw data
- ANALYSIS/RESULTS/brain/master_dat.RData: the brain size results, can be loaded directly into R
- ANALYSIS/RESULTS/clutch size/master_dat.RData: the clutch size results, can be loaded directly into R
- ANALYSIS/RESULTS/developmental time/master_dat.RData: the developmental time results, can be loaded directly into R
- ANALYSIS/RESULTS/diet/master_dat.RData: the diet results, can be loaded directly into R
- ANALYSIS/RESULTS/figures/brain vs life model 1.pdf: pdf of the brain size results of model 1
- ANALYSIS/RESULTS/figures/brain vs life model 2.pdf: pdf of the brain size results of model 2
- ANALYSIS/RESULTS/figures/phylogentic tree - with icons.pdf: pdf for figure 2 in the paper
- ANALYSIS/RESULTS/figures/phylogentic tree - with icons.png: png for figure 2 in the paper
- ANALYSIS/RESULTS/figures/phylogentic tree - with icons.svg: svg for figure 2 in the paper
- ANALYSIS/RESULTS/figures/phylogentic tree - with names.pdf: pdf with all names
- ANALYSIS/RESULTS/figures/slopes model 2.pdf: pdf for figure 3
- ANALYSIS/RESULTS/figures/slopes model 2.png: png for figure 3
- ANALYSIS/RESULTS/insularity/master_dat.RData: the insularity results, can be loaded directly into R
- ANALYSIS/RESULTS/latitude/.gitignore: ignoring large map pdf
- ANALYSIS/RESULTS/latitude/master_dat.RData: the latitude results, can be loaded directly into R
- ANALYSIS/RESULTS/life expectancy/master_dat.RData: the life expectancy results, can be loaded directly into R
- ANALYSIS/RESULTS/life expectancy/checklist_2021-03-23 10:57:36 results no sex.csv: a list of all species with a column (keep_GO) where the results of the manual model fit check are scored
- ANALYSIS/RESULTS/life expectancy/.gitignore: ignoring large model output folders
- ANALYSIS/RESULTS/models/README.md: overview
- ANALYSIS/RESULTS/models/.gitignore: ignoring large model files
- ANALYSIS/RESULTS/weight/.gitignore: ignoring large model files and sensitive results
- ANALYSIS/RESULTS/weight/master_dat.RData: the body weight results, can be loaded directly into R

------------------------------------------------

**Meta data:**

- ANALYSIS/DATA/Andrew Iwaniuk/data.csv
	- column A: genus name
	- column B: species name
	- column C: individual ID
	- column D: sex
	- column E: not used
	- column F: brain weight (grams)
- ANALYSIS/DATA/cleaned weights/WeightMean.csv
	- species: standardised species name
	- varval: variable value, body weight in grams
	- other columns: full taxonomy
- ANALYSIS/DATA/diet/diet data.xlsx
	- column A: standardised species name
	- column B-J: food types, possible values are described in the meta data tab
	- source: last name expert or reference to paper, multiple entries possible
- ANALYSIS/DATA/other litterature/data.csv
	- species: standardised species name
	- source: reference to book, website or publication
	- min_foraging_flock: minimal foraging flock size recorded
	- max_foraging_flock: maximum normal foraging flock size recorded
	- extreme_foraging_flock: foraging flock size at e.g. clay-lick or fruiting tree
	- communal_roost: whether or not the species has a communal roost 
	- immature_foraging_flock: the size of immature flocks if recorded
	- breeding_flock: the size is breeding flocks if recorded
	- min_n_eggs: minimal number of eggs recorded
	- max_n_eggs: maximum number of eggs recorded
	- incubation_days: incubation time in days
	- fledging_days: fledging period in days (from hatch to leaving nest)
	- nestling_period_days: see fledging_days (some books use different terminology)
	- fed_by_parent_months: how long parents feed offspring after fledging in months
	- fledging_period_days: not used
	- foraging_flock_size: not used
	- habitat: not used
	- sexually_different_marking: whether or not species have different colouration between sexed
	- diet: which food sources are used (we did not use this source, but contacted experts)
	- nesting: where species nest
	- flocking: not used
	- first_repdroduction: not used
	- notes: not used
- ANALYSIS/DATA/taxonomy/IUCN sub species taxonomy.csv
	- original_species: sub-species names used in zoos; note that this column can have multiple entries per cell, the repo includes a function to clean this up and compile a data frame that can be used for standardisation
	- species: the standardised IUCN species-level name
- ANALYSIS/DATA/taxonomy/IUCN translation list - final.csv
	- original_species: species names used in data bases and zoos
	- species: the standardised IUCN species-level name
	- order: the standardised IUCN order-level name
	- source: how the IUCN taxonomy was found: exact_match - found exactly the same on IUCN, fuzzy_match - found with few characters shuffled on ICUN, manual_avibase - found manually on Avibase, synonym_match - match to synonym listed by IUCN
	- notes: when species not found this explains why - often a species is a hybrid or is extinct
- ANALYSIS/RESULTS/AFR/5th percentile results.csv
	- column A: not used
	- species: the standardised IUCN species-level name
	- afr_5th_percentile: the fifth percentile of ages when individuals were observed to reproduced for the first time (in days)
- ANALYSIS/RESULTS/brain/master_dat.RData
	- species: the standardised IUCN species-level name
	- log_mean_brain_size: the mean of the natural logarithm of the predicted brain size from the Bayesian model
	- log_SE_brain_size: the standard error (standard deviation) of the natural logarithm of the predicted brain size from the Bayesian model
- ANALYSIS/RESULTS/clutch size/master_dat.RData
	- species: the standardised IUCN species-level name
	- clutch_size_n: the average number of eggs per clutch
- ANALYSIS/RESULTS/developmental time/master_dat.RData	
	- species: the standardised IUCN species-level name
	- fledging_age_days: from hatch to fledge in days
	- incubation_days: incubation period in days
- ANALYSIS/RESULTS/diet/master_dat.RData
	- species: the standardised IUCN species-level name
	- diet_protein: protein content of diet ranging from low (1) to high (3), ordered categorical
- ANALYSIS/RESULTS/insularity/master_dat.RData
	- species: the standardised IUCN species-level name
	- insularity: whether a species is only found on islands (1) or not (0)
- ANALYSIS/RESULTS/latitude/master_dat.RData
	- species: the standardised IUCN species-level name
	- max_lat: the maximum absolute latitude (furthest away from equator) a species range covers
- ANALYSIS/RESULTS/life expectancy/master_dat.RData
	- species: the standardised IUCN species-level name
	- log_mean_life_exp: the mean of the natural logarithm of the predicted life expectancy from the Bayesian model
	- log_SE_life_exp: the standard error (standard deviation) of the natural logarithm of the predicted life expectancy from the Bayesian model
- ANALYSIS/RESULTS/life expectancy/checklist_2021-03-23 10:57:36 results no sex.csv
	- species: the standardised IUCN species-level name
	- column B-F: not used
	- keep_GO: whether or not the model full-filled all requirements to be included (convergence)
- ANALYSIS/RESULTS/weight/master_dat.RData
	- species: the standardised IUCN species-level name
	- log_mean_body_weight: the mean of the natural logarithm of the predicted body weight from the Bayesian model
	- log_SE_body_weight: the standard error (standard deviation) of the natural logarithm of the predicted body weight from the Bayesian model

------------------------------------------------

**Maintainers and contact:**

Please contact Simeon Q. Smeele, <ssmeele@ab.mpg.de>, if you have any questions or suggestions. 




