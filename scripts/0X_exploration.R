##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Exploration #################################################
#-------------------------------------------------------------------------

#Annual trends of seal counts ############

annual_summary <- PRNS_seals %>% 
  #Filter for only living individuals from the breeding/pupping season
  filter(age == "ADULT" |
         age == "PUP") %>%
  mutate(year = year(date)) %>% 
  group_by(year, age, subsite) %>% 
  summarise(max_number = max(count), .groups = "drop")

ggplot(annual_summary, 
       aes(x=year, y=max_number, 
           fill = factor(age, levels = c("PUP", "ADULT"))))+
  #Stacked bar chart
  geom_bar(stat = "identity", position="stack")+
  facet_wrap(facets = "subsite", ncol=1)+ #facet by site
  theme_few()+
  labs(y = "Maximum # Individuals Documented", x="Year", fill = "Age Class")+
  scale_fill_manual(values = c("#22A884FF","#440154FF"))

#Trends of Seal Pup Timing ##################

timing_summary <- PRNS_seals %>% 
  filter(age == "PUP") %>% 
  mutate(day = yday(date),
         year = factor(year(date)))

ggplot(timing_summary, aes(x=day, y=count, color = year))+
  geom_point()+
  facet_wrap(facets = "subsite", ncol = 1)+
  scale_color_viridis_d()+
#  geom_smooth(method = "gam")+
  theme_few()+
  labs(y = "# Pups Observed", x="Day of Year", fill = "Year")

  