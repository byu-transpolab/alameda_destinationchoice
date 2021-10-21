library(targets)
# This is an example _targets.R file. Every
# {targets} pipeline needs one.
# Use tar_script() to create _targets.R and tar_edit()
# to open it again for editing.
# Then, run tar_make() to run the pipeline
# and tar_read(summary) to view the results.
source("R/datamaker.R")
source("R/methods.R")
source("R/modeling.R")

# Define custom functions and other global objects.
# This is where you write source(\"R/functions.R\")
# if you keep your functions in external scripts.
summ <- function(dataset) {
  summarize(dataset, mean_x = mean(x))
}

# Set target-specific options such as packages.
tar_option_set(packages = c(
  "tidyverse", "osmdata", "sf", "ggmap", "leaflet",
  "tidycensus", "modelsummary", "knitrProgressBar",
  "mlogit", "broom" 
))

this_crs <- 2227 # EPSG:2227 – NAD83 / California zone 3 (ftUS)
bb <- osmdata::getbb("Alameda County, California", format_out = "polygon")
set.seed(42)
n_obs <- 20000
n_alts <- 10

# End this file with a list of target objects.
list(
  # get parks and attributes
  tar_target(parksfile, "data/bayarea_parks.geojson", format = "file"),
  tar_target(parks, get_parks(parksfile, this_crs)),
  tar_target(playgrounds, get_playgrounds(bb, this_crs)),
  tar_target(trails, get_trails(bb, this_crs)),
  tar_target(pitches, get_pitches(bb, this_crs)),
  tar_target(attributed_parks, attribute_parks(parks, playgrounds, pitches, trails)),
  
  
  # build estimation dataset
  tar_target(park_flows, read_rds("data/park_flows.rds")),
  tar_target(acs, get_acsdata()),
  
  tar_target(shortest_paths_7z, "data/shortest_paths.7z", format = "file"),
  tar_target(path_files, extract_paths(shortest_paths_7z)),
  tar_target(shortest_paths_street_7z, "data/shortest_paths_street.7z", format = "file"),
  tar_target(street_path_files, extract_paths_street(shortest_paths_street_7z)),
  tar_target(distance_df, get_shortest_paths(path_files)),
  tar_target(street_distances, get_shortest_paths(street_path_files)),
  
  # slow streets
  tar_target(slowstreets_gj, "data/slow_streets.geojson", format = "file"),
  tar_target(slowstreets, st_read(slowstreets_gj, quiet = TRUE) %>% st_transform(this_crs) ),
  tar_target(street_parks, make_street_parks(slowstreets)),
  
  tar_target(logitdata, make_logitdata(park_flows, attributed_parks, distance_df, acs, n_obs, n_alts)),
  
  # modeling
  tar_target(estim, make_estim(logitdata)),
  tar_target(base_models, estimate_base_models(estim)),
  tar_target(grouped_models, estimate_grouped_models(estim)),
  #tar_target(split_models, estimate_all_splits(estim)),
  #tar_target(split_dat, make_split_dat(split_models)),
  
  
  tar_target(logsums, make_logsums(distance_df, attributed_parks, street_distances, street_parks, 
                         base_models, acs)),
  tar_target(benefits, make_benefits(logsums))
  
  
)
