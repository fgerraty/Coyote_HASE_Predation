##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Exploration #################################################
#-------------------------------------------------------------------------

seals <- read_csv("data/clean/seals.csv") %>% 
  mutate(subsite = factor(subsite, levels = c("MC", "DE", "DP", "BL")))


#Annual trends of seal counts ############

annual_summary <- seals %>% 
  #Filter for only living individuals from the breeding/pupping season
  mutate(year = year(date)) %>% 
  group_by(year, age, subsite) %>% 
  summarise(max_number = max(count, na.rm = TRUE), .groups = "drop")

ggplot(annual_summary, 
       aes(x=year, y=max_number, 
           fill = factor(age, levels = c("pup", "adult"))))+
  #Stacked bar chart
  geom_bar(stat = "identity", position="stack")+
  facet_wrap(facets = "subsite", 
             ncol=1, scales = "free_y", 
             labeller = labeller(subsite = site_names))+ #facet by site
  theme_few()+
  labs(y = "Maximum # Individuals Documented Per Year", x="Year", fill = "Age Class")+
  scale_fill_manual(values = c("#22A884FF","#440154FF"))


ggsave("output/annual_summary.png", width = 7.5, height = 10, units = "in")


#Trends of Seal Pup Timing ##################

site_names <- c(MC = "MacKerricher", DE = "Drake's Estero", DP = "Double Point", BL = "Bolinas Lagoon")

timing_summary <- seals %>% 
  filter(age == "pup") %>% 
  mutate(day = yday(date),
         year = factor(year(date)),
         count = ifelse(is.na(count), 0, count)) 

ggplot(timing_summary, aes(x=day, y=count, color = year))+
  geom_point()+
  facet_wrap(facets = "subsite", 
             ncol = 1, scales = "free_y", 
             labeller = labeller(subsite = site_names))+
  scale_color_viridis_d()+
  theme_few()+
  labs(y = "# Pups Observed", x="Day of Year", fill = "Year")

ggsave("output/timing_summary.png", width = 7.5, height = 10, units = "in")

  