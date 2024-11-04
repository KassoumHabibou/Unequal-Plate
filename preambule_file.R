
############################################################################
######### Preambule  #######################################################
############################################################################

# This file aim at importing and exporting the DHS file so it can be imported 
# in Python
######################## Importing library and external files ##############
## List of required packages
required_packages <- c("tidyverse", "ipumsr","here","foreign")

### Check if packages are installed
missing_packages <- setdiff(required_packages, installed.packages()[,"Package"])

### Install missing packages
if (length(missing_packages) > 0) {
  install.packages(missing_packages)
}

### Load all packages (except "plyr" to prevent conflicts)


lapply(required_packages, library, character.only = TRUE)


######################## Importing datasets ####################################

# unzip the file
gzFile<- paste0(here::here(),"/input/individual_data/idh_zip/idhs_00021.dat.gz")
R.utils::gunzip(gzFile, remove = TRUE, ext="gz", FUN=gzfile)

# Read DDI file and micro data using ipumsr package
ddi <- read_ipums_ddi(paste0(here::here(),"/input/individual_data/ddi/idhs_00021.xml"))
df <- read_ipums_micro(ddi, data_file = paste0(here::here(),"/input/individual_data/idh_zip/idhs_00021.dat"))


# Select relevant columns from the dataframe


# Exporting to stata file
haven::write_dta(df, paste0(here::here(),"/input/individual_data/dat/idhs_00021.dta"),version = 15)



