##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------

####################################
# PART 1: Import and Clean Data ####
####################################

# PART 1A: Import and clean PORE harbor seal population data (2 sites) ---------

PRNS_seals <- read_excel("data/raw/PRNS_HarborSealCounts_2010_2022.xlsx") %>% 
  clean_names() %>% 
  mutate(time = hms::as_hms(time))



