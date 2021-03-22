#Download census data

library(tidyverse)
library(tidycensus)
library(sf)

vars<- load_variables(2019, "acs5", cache = TRUE)

#Disability
disability_var<-vars %>%
  filter(substr(name,1,6)=="C18108")

disability<-get_acs(geography="tract",state="GA",var=disability_var$name,
                    year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop=C18108_001,
         disable_pop=C18108_003+C18108_004+C18108_007+C18108_008+
           C18108_011+C18108_012,
         disable_pct=round(disable_pop/total_pop*100,1)) %>%
  select(GEOID,total_pop,disable_pop,disable_pct)

#No vehicle
vehicle_var<-vars %>%
  filter(substr(name,1,6)=="B08014")

vehicle<-get_acs(geography="tract",state="GA",var=vehicle_var[1:2,]$name,
                    year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_veh=B08014_001,
         noveh_pop=B08014_002,
         noveh_pct=round(noveh_pop/total_pop_veh*100,1)) %>%
  select(GEOID,total_pop_veh:noveh_pct)

#Non-white
raceeth_var<-vars %>%
  filter(substr(name,1,6)=="B03002")

raceeth<-get_acs(geography="tract",state="GA",var=raceeth_var[c(1,3),]$name,
                 year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_race=B03002_001,
         nonwht_pop=B03002_001-B03002_003,
         nonwht_pct=round(nonwht_pop/total_pop_race*100,1)) %>%
  select(GEOID,total_pop_race:nonwht_pct)

#English less than well
lang_var<-vars %>%
  filter(substr(name,1,6)=="C16001")
lang_var_lesswell<-lang_var %>%
  filter(str_detect(label,"less than")) %>%
  bind_rows(lang_var[1,])

lang_lesswell<-get_acs(geography="tract",state="GA",var=lang_var_lesswell$name,
                 year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_lang=C16001_001,
         engnotwell_pop=rowSums(.[4:15]),
         engnotwell_pct=round(engnotwell_pop/total_pop_lang*100,1)) %>%
  select(GEOID,total_pop_lang:engnotwell_pct)

#Age 65 and older
age65<-get_acs(geography="tract",state="GA",var=c("C18108_001","C18108_010"),
                       year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_age65=C18108_001,
         age65_pop=C18108_010,
         age65_pct=round(age65_pop/total_pop_age65*100,1)) %>%
  select(GEOID,total_pop_age65:age65_pct)

#Poverty
povrate<-get_acs(geography="tract",state="GA",var=c("B17020_001","B17020_002"),
               year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_pov=	B17020_001,
         pov_pop=	B17020_002,
         pov_pct=round(pov_pop/total_pop_pov*100,1)) %>%
  select(GEOID,total_pop_pov:pov_pct)

#No HS Diploma
lesshsdip_rate<-get_acs(geography="tract",state="GA",var=c("B16010_001","B16010_002"),
                 year=2019) %>%
  select(-moe) %>%
  pivot_wider(names_from=variable,values_from=estimate) %>%
  mutate(total_pop_hsdip=	B16010_001,
         lesshsdip_pop=	B16010_002,
         lesshsdip_pct=round(lesshsdip_pop/total_pop_hsdip*100,1)) %>%
  select(GEOID,total_pop_hsdip:lesshsdip_pct)

#Combine
census_all<-disability %>%
  left_join(vehicle) %>%
  left_join(raceeth) %>%
  left_join(lang_lesswell) %>%
  left_join(age65) %>%
  left_join(povrate) %>%
  left_join(lesshsdip_rate)

tracts<-get_acs(geography="tract",state="GA",
                  var=c("B16010_001"),year=2019,geometry=TRUE) %>%
  select(GEOID)

tracts_data<-tracts %>% left_join(census_all) %>%
  st_transform(4326)

st_write(tracts_data,"data/census_area.gpkg") 
st_write(tracts_data,"data/census_area.geojson")
