library(tidyverse)
library(here)
library(sf)

# Read parks to get ID column
parks <- st_read("data/bayarea_parks.geojson")

# Read streetlight data (cannot be shared)
park_flows <- read_csv(here("data/Master_BlockGroups_Final071519.csv"), 
         col_types = cols()) %>% 
  # need to make block group id properly formatted.
  transmute(
    park = as.character(`Zone Name`),
    home = paste0("0", round(`Block Group ID`)),
    county = substr(home, 0, 5),
    attractions = `Zone Traffic (StL Index)`,
    d_share = `Percent by Home Location`
  ) %>%
  
  # only keep bgs in the area
  filter(county %in% c("06001")) %>%
  # and only keep parks that we are looking at
  filter(park %in% parks$id) %>%
  
  mutate(flow = attractions * d_share) %>%
  group_by(home) %>%
  mutate(
    productions = sum(flow),
    o_share = flow / productions
  ) 

write_rds(park_flows, "data/park_flows.rds")
