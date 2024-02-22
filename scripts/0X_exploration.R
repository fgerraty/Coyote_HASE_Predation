##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 0X: Exploration #################################################
#-------------------------------------------------------------------------

seals <- read_csv("data/clean/seals.csv") %>% 
  mutate(subsite = factor(subsite, levels = c("MC", "DE", "DP", "BL")))

site_names <- c(MC = "MacKerricher", DE = "Drake's Estero", DP = "Double Point", BL = "Bolinas Lagoon")

#Annual trends of seal counts ############

annual_summary <- seals %>% 
  #Filter for only living individuals from the breeding/pupping season
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


ggsave("output/extra_figures/annual_summary.png", width = 7.5, height = 10, units = "in")


#Trends of Seal Pup Timing ##################

timing_summary <- seals %>% 
  filter(age == "pup") %>% 
  mutate(day = yday(date),
         year = factor(year(date)),
         count = ifelse(is.na(count), 0, count)) %>% 
  filter(day>80 & day <165)

ggplot(timing_summary, aes(x=day, y=count))+
  geom_point(alpha = .7, shape=16)+
  geom_vline(data = MC_seal_mortality, 
             aes(xintercept = day, size=count, color = "red")) +
  geom_vline(data = PRNS_seal_mortality, 
             aes(xintercept = day, size=count, color = "red")) +
  facet_wrap(facets = "subsite", 
             ncol = 1, scales = "free_y", 
             labeller = labeller(subsite = site_names))+
  theme_few()+
  labs(y = "# Pups Observed", x="Day of Year")+
  geom_smooth(method = "gam", color = "black")+
  scale_size_continuous(range = c(.5,1.5))+
  theme(legend.position = "none")

ggsave("output/extra_figures/timing_summary.png", width = 7.5, height = 10, units = "in")

  

MC_timing <- timing_summary %>% 
  filter(subsite == "MC")

ggplot(MC_timing, aes(x=day, y=count))+
  geom_point(alpha = .7, shape=16)+
  geom_vline(data = MC_seal_mortality, 
             aes(xintercept = day, 
                 size=count 
                 
                 ),
             color = "red") +
  theme_few()+
  labs(y = "# Pups Observed (all years)", x="Day of Year", size = "# Pups Killed\nper Julian Date\n(sum of all years)")+
  geom_smooth(method = "gam", color = "black")+
  scale_size_continuous(range = c(.5,1.5), 
                        breaks= c(1,2,3,4),
                        labels = c(1,2,3,4))+
  guides(size=guide_legend(override.aes=list(colour="red")))+
  coord_cartesian(ylim = c(-2,50))


+
#  theme(legend.position = "none")+
#  scale_color_viridis()
