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
  facet_wrap(facets = "subsite", ncol=1, scales = "free_y")+ #facet by site
  theme_few()+
  labs(y = "Maximum # Individuals Documented Per Year", x="Year", fill = "Age Class")+
  scale_fill_manual(values = c("#22A884FF","#440154FF"))

#Trends of Seal Pup Timing ##################

timing_summary <- seals %>% 
  filter(age == "pup") %>% 
  mutate(day = yday(date),
         year = factor(year(date)),
         count = ifelse(is.na(count), 0, count)) 

ggplot(timing_summary, aes(x=day, y=count, color = year))+
  geom_point()+
  facet_wrap(facets = "subsite", ncol = 1, scales = "free_y")+
  scale_color_viridis_d()+
  theme_few()+
  labs(y = "# Pups Observed", x="Day of Year", fill = "Year")

  