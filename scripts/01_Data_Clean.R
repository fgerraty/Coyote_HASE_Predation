##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 01: Data Clean ##################################################
#-------------------------------------------------------------------------

###############################################
# PART 1: Import and Clean Seal Count Data ####
###############################################

# Import and clean PORE harbor seal population data (3 sites) ---------

PRNS_seals <- read_excel("data/raw/PRNS_HarborSealCounts_2010_2022.xlsx") %>% 
  clean_names() %>% 
  mutate(time = hms::as_hms(time), #fix time format
         #replace values in age column with lower case
         age = replace(age, age == "ADULT", "adult"),
         age = replace(age, age == "PUP", "pup"),
         year = year(date)) %>%  
  #Filter for only living individuals from the breeding/pupping season
  filter(age == "adult" |
         age == "pup")


# Import and clean Pre-2018 MacKerricher harbor seal population data (1 site) ---------
MC_seals_pre2018 <- read_excel("data/raw/Mackerricher_HASE_Counts_Pre2018.xlsx") %>% 
  clean_names() %>% 
  filter(location == "R") %>% 
  select(date, time, adult, pup) %>%  #select relevant columns
  pivot_longer(cols = adult:pup,
               names_to = 'age', 
               values_to = "count") %>% 
  mutate(time = hms::as_hms(time), #fix time format
         subsite = "MC",
         year = year(date))

# Import and clean MacKerricher harbor seal population data (1 site) ---------
MC_seals_post2018 <- read_excel("data/raw/MacKerricher_seal_summary_post2018.xlsx") %>% 
  select(year, annual_adult_max, annual_pup_max) %>% 
  pivot_longer(cols = c(annual_adult_max, annual_pup_max), 
               names_to = "age", values_to = "count") %>% 
  mutate(age = if_else(age =="annual_adult_max", "adult", "pup"),
         subsite = "MC", date = NA, time = NA) 

#Combine all seal data together and export ------------------------------------
seals <- rbind(PRNS_seals, MC_seals_pre2018, MC_seals_post2018)

write_csv(seals, "data/clean/seals.csv")
  


###################################################
# PART 1: Import and Clean Seal Predation Data ####
###################################################

MC_seal_mortality <- read_excel("data/raw/MacKerricher_mortality.xlsx") %>% 
  clean_names() %>% 
  mutate(date = ymd(observation_date),
         day = yday(date),
         year = year(date),
         subsite = "MC") %>% 
  select(subsite, day, year) %>% 
  group_by(subsite, day, year) %>% 
  summarize(count = n(), .groups = "drop")

PRNS_seal_mortality <- read_excel("data/raw/PRNS_mortality.xlsx") %>% 
  clean_names() %>% 
  filter(type_of_event %in% c("Observed predation", "Suspected predation attempt and Scavenging")) %>% 
  mutate(subsite = site, 
         day = yday(start_date),
         year = year(start_date)) %>% 
  select(subsite, day, year) %>% 
  group_by(subsite, day, year) %>% 
  summarize(count = n(), .groups = "drop")

#Combine all seal data together and export ------------------------------------
mortality <- rbind(MC_seal_mortality, PRNS_seal_mortality)

write_csv(mortality, "data/clean/mortality.csv")
