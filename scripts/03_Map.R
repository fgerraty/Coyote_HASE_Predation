##########################################################################
# Coyotes hunt harbor seals along the California Coast  ##################
# Author: Frankie Gerraty (frankiegerraty@gmail.com; fgerraty@ucsc.edu) ##
##########################################################################
# Script 03: Map #########################################################
#-------------------------------------------------------------------------

# Import Data ####

sites <- read_csv("data/raw/sites.csv")
  
  
states <- map_data("state")
ca_df <- subset(states, region == "california")



ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgrey")+
  geom_point(data = sites, aes(x=longitude, y=latitude), color = "red")+
  coord_sf(crs = 4326)+
  theme_void()


ggplot() + 
  coord_fixed(1.3) + 
  geom_polygon(data = ca_df, mapping = aes(x = long, y = lat, group = group), 
               color = "black", fill = "lightgrey")+
  geom_point(data = sites, aes(x=longitude, y=latitude), color = "red")+
  coord_sf(crs = 4326,
           xlim = c(-124, -122),
           ylim = c(37, 40),
           expand = FALSE)+
  theme_void()
