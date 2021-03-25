# Under the hood data prep
# MV Evans (mv.evans.phd@gmail.com)
# Originated March 24 2021
# Last Updated March 24 2021

library(sf)
library(dplyr)

#Census polygons ####
service.area <- st_read("data/nega_servicearea_cty.gpkg",stringsAsFactors=FALSE) %>%
  select(GEOID, NAME)

census.vars <- st_read("data/census_area.gpkg",stringsAsFactors=FALSE) %>%
  #select seven variables we need
  select(GEOID, total_pop,disable_pop, noveh_pop, nonwht_pop, engnotwell_pop, age65_pop, pov_pop, lesshsdip_pop) %>%
  #filter to only the counteies we need
  mutate(county_GEOID = substr(GEOID, 1,5)) %>%
  filter(county_GEOID %in% service.area$GEOID) %>%
  #add county name
  left_join(st_drop_geometry(service.area), by = c("county_GEOID" = "GEOID")) %>%
  rename(County = NAME) %>%
  #create fancy popup box
  mutate(popup_text = paste0("<strong>Total population: </strong>", total_pop, "<br>",
                             "<strong>Civilians with Disabilities: </strong>", disable_pop, "<br>",
                             "<strong>Households without Vehicles: </strong>", noveh_pop, "<br>",
                             "<strong>Minority Population: </strong>", nonwht_pop, "<br>",
                             "<strong>Persons(5+) who Speak English Less Than Well: </strong>", engnotwell_pop, "<br>",
                             "<strong>Persons Over 65: </strong>", age65_pop, "<br>",
                             "<strong>Persons Below Poverty: </strong>", pov_pop, "<br>",
                             "<strong>Persons (25+) without a Highschool Diploma: </strong>", lesshsdip_pop, "<br>"))

# Spatial Points #####
vaccine.prov <- read.csv("data/providers_geocoded_2021_03_19.csv", stringsAsFactors = F) %>%
  st_as_sf(., coords = c("lon", "lat"), crs = st_crs(census.vars)) %>%
  mutate(Type = "Vaccine Provider")

# service providers still needs to be geocoded

#combine into one dataset with service providers
services.all <- vaccine.prov %>%
  mutate(id = paste(substr(Type,1,1), formatC(1:n(), width = 3, format = "d", flag = "0"), sep = "_"))

#create textbox
services.all$textbox <- paste0("<strong>Type: </strong>", services.all$Type, "<br>",
                               "<strong>Facility: </strong>", services.all$Facility, "<br>",
                               "<strong>Population Served: </strong>", services.all$Population.Served, "<br>",
                               "<strong>Phone: </strong>", services.all$Phone.Number, "<br>",
                               "<strong>Address: </strong>", services.all$Street.Address, "<br>",
                               "<strong>City: </strong>", services.all$City, "<br>",
                               "<strong>County: </strong>", services.all$County, "<br>",
                               "<strong>ZIP Code: </strong>", services.all$ZIP.Code, "<br>",
                               "<strong>Contact Email: </strong>", services.all$Contact.Email, "<br>")
