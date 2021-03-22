#Geocode providers

library(tidyverse)
library(ggmap)

#register_google("xxxx")

providers<-read_csv("data/Public Vaccine Providers 03122021.csv") %>%
  mutate(full_add=paste(`Street Address`,", ",City,", GA ",`ZIP Code`,sep=""))

providers_xy<-providers %>%
  mutate_geocode(full_add)

write_csv(providers_xy,"data/providers_geocoded_2021_03_19.csv")
