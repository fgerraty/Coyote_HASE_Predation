##########################################################################
# Harbor Seal Pup Predation by Coyotes  ##################################=
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 02: MacKerricher Plots ##########################################
#-------------------------------------------------------------------------

#PART 1: MacKerricher Data Manipulation-----------------------------------

#MacKerricher abundance data
MC_seals <- read_csv("data/clean/seals.csv") %>% 
  filter(subsite == "MC", #filter for MC site
         year > 2010) %>% #remove oldest data
  #Count max number of adults and pups documented each year
  group_by(year, age) %>% 
  summarise(max_number = max(count, na.rm = TRUE), .groups = "drop")

#MacKerricher mortality data
MC_mortality <- read_csv("data/clean/mortality.csv") %>% 
  filter(subsite == "MC") %>% 
  group_by(year) %>% 
  summarise(max_number = sum(count)) %>% 
  mutate(age = "dead_pup")

#Combine alive and dead seal counts
MC_summary <- rbind(MC_seals, MC_mortality)
         


#PART 1: MacKerricher Abundance Plot -----------------------------------

ggplot(MC_summary, 
       aes(x=year, y=max_number, 
           fill = factor(age, 
                         levels = c("dead_pup", "pup", "adult"))))+
  #Stacked bar chart
  geom_bar(stat = "identity", position="stack")+
  theme_few()+
  labs(y = "Maximum # Individuals Documented Per Year", 
       x="Year", fill = "Age Class")+
  scale_fill_manual(values = c("red", "#22A884FF","#440154FF"), labels = c("Coyote-\nKilled Pup", "Pup", "Adult"))+
  theme(legend.position = c(0.9, 0.83))

ggsave("output/extra_figures/MC_annual_summary.png", width = 7, height = 4.5, units = "in")
  


#PART 2: MacKerricher Timing Plot -----------------------------------

MC_timing_summary <- read_csv("data/clean/seals.csv") %>% 
  filter(subsite == "MC", #filter for MC site
         year > 2010, #remove oldest data
         age == "pup") %>% 
  mutate(day = yday(date),
         year = factor(year(date)),
         count = ifelse(is.na(count), 0, count))

MC_timing_mortality <- read_csv("data/clean/mortality.csv") %>% 
  filter(subsite == "MC") %>% 
  group_by(day) %>% 
  summarise(count = sum(count))

ggplot(MC_timing_summary, aes(x=day, y=count))+
  geom_point(alpha = .7, shape=16)+
  geom_vline(data = MC_timing_mortality, 
             aes(xintercept = day, size=count), color = "red") +
  theme_few()+
  labs(y = "# Pups Observed (all years)", x="Day of Year", size = "# Coyote-Killed\nPups per Julian Day\n(sum of all years)")+
  geom_smooth(method = "gam", color = "black")+
  scale_size_continuous(range = c(.5,1.5), 
                        breaks= c(1,2,3,4),
                        labels = c(1,2,3,4))+
  guides(size=guide_legend(override.aes=list(colour="red")))+
  coord_cartesian(ylim = c(-2,50), xlim = c(92,165))+
  guides(size = guide_legend(nrow=1,
         label.position = "top"))+
  theme(legend.position = c(0.865, 0.83))

ggsave("output/extra_figures/MC_timing_summary.png", width = 7, height = 4.5, units = "in")
