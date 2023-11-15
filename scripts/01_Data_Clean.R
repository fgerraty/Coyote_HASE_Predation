##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------

####################################
# PART 1: Import and Clean Data ####
####################################

# Import and clean PORE harbor seal population data (3 sites) ---------

PRNS_seals <- read_excel("data/raw/PRNS_HarborSealCounts_2010_2022.xlsx") %>% 
  clean_names() %>% 
  mutate(time = hms::as_hms(time), #fix time format
         #replace values in age column with lower case
         age = replace(age, age == "ADULT", "adult"),
         age = replace(age, age == "PUP", "pup")) %>%  
  #Filter for only living individuals from the breeding/pupping season
  filter(age == "adult" |
         age == "pup")


# Import and clean MacKerricher harbor seal population data (1 site) ---------
MC_seals <- read_excel("data/raw/Mackerricher_HASE_Counts_Pre2018.xlsx") %>% 
  clean_names() %>% 
  filter(location == "R") %>% 
  select(date, time, adult, pup) %>%  #select relevant columns
  pivot_longer(cols = adult:pup,
               names_to = 'age', 
               values_to = "count") %>% 
  mutate(time = hms::as_hms(time), #fix time format
         subsite = "MC")

#Combine all seal data together and export ------------------------------------
seals <- rbind(PRNS_seals, MC_seals) 

write_csv(seals, "data/clean/seals.csv")
  
