#' Get the parks out of the CPAD file
#' 
#' 
get_parks <- function(parks_file, this_crs){
  not_parks <- c("3455", "15886", "13243")
  
  st_read(parks_file, quiet = TRUE) %>%
    filter(COUNTY == "Alameda") %>%
    transmute(
      id = as.character(UNIT_ID), name = UNIT_NAME, 
      access = factor(ACCESS_TYP, levels = c("Open Access", "No Public Access", 
                                             "Restricted Access")),
      acres = ACRES, type = DES_TP) %>%
    filter(!id %in% not_parks) %>%
    filter(access == "Open Access") %>%
    select(id, access, acres, type)  %>%
    st_transform(this_crs)
}


get_playgrounds <- function(bb, this_crs){
  playground_osm <- opq(bb) %>% # specify boundary for query
    add_osm_feature(key = "leisure", value = "playground") %>% # specify which kinds of data we want
    osmdata_sf() %>% # get a list of sf data frames for these tags
    trim_osmdata(bb, exclude = TRUE)
  
  rbind(
    # convert polygons to centroids
    playground_osm$osm_polygons %>%
      st_transform(this_crs) %>%
      st_centroid() %>%
      select(osm_id),
    # get points
    playground_osm$osm_points %>%
      st_transform(this_crs) %>%
      select(osm_id)
    # no multipolygons or polylines that need to be included
  ) %>%
    mutate(playground = TRUE)
}

get_trails <- function(bb, this_crs){
  trails_osm <- opq(bb) %>% # specify boundary for query
    add_osm_feature(key = "highway", value = c("footway", "cycleway", "path")) %>% 
    osmdata_sf() %>% 
    trim_osmdata(bb, exclude = FALSE)
  
  rbind(
    # points are nodes, so we can skip them.
    # lines
    trails_osm$osm_lines %>%
      st_transform(this_crs) %>%
      select(osm_id, bicycle, foot, horse),
    
    # circular paths get converted to polygons, so we need to 
    # turn them back into lines.
    trails_osm$osm_polygons %>%
      st_transform(this_crs) %>%
      st_boundary() %>%
      select(osm_id, bicycle, foot, horse)
  ) %>%
    mutate(length = st_length(.))
}


get_pitches <- function(bb, this_crs){
  pitches_osm <- opq(bb) %>% # specify boundary for query
    add_osm_feature(key = "leisure", value = "pitch") %>% 
    osmdata_sf() %>% 
    trim_osmdata(bb, exclude = TRUE)
  
  rbind(
    # convert polygons to centroids
    pitches_osm$osm_polygons %>%
      st_transform(this_crs) %>%
      st_centroid() %>%
      select(osm_id, sport),
    # get points
    pitches_osm$osm_points %>%
      st_transform(this_crs) %>%
      select(osm_id, sport)
    # no multipolygons or polylines that need to be included
  ) %>%
    filter(sport != "golf") %>%
    mutate(
      sport = case_when(
        grepl("football", sport) | grepl("soccer", sport) ~ "football / soccer",
        grepl("baseball", sport) | grepl("softball", sport) ~ "baseball",
        grepl("basketball", sport) ~ "basketball",
        grepl("tennis", sport) ~ "tennis",
        grepl("volleyball", sport) ~ "volleyball",
        TRUE ~ "other_pitch"
      )
    )
}

get_shops <- function(bb, this_crs){
  retail <- opq(bb) %>%
    add_osm_feature(key = "landuse", value = "retail") %>%
    osmdata_sf() %>% 
    trim_osmdata(bb, exclude = TRUE)
  
  shops <- lapply(c("bar", "cafe", "fast_food", "restaurant", "bank", "pharmacy"), function(type){
    get_shoptype(type, bb, this_crs)
  }) %>%
    bind_rows()
  
    
}

get_shoptype <- function(type, bb, this_crs){
  o <- opq(bb) %>% add_osm_feature(key = "amenity", value = type) %>%
    osmdata_sf() %>% trim_osmdata(bb, exclude = TRUE)
  
  o$osm_points %>%
    st_transform(this_crs) %>%
    select(osm_id, amenity, geometry)
}

attribute_parks <- function(parks, playgrounds, pitches, trails, shops){
  # Append attributes to park frame ===============
  parks_with_playgrounds <-  parks %>%  
    # compute yeo-johnson transformation on acres
    # determine if a playground point is inside the park polygon boundary
    st_join(playgrounds %>% select(playground), st_intersects)  %>%
    # a park with multiple playgrounds will join multiple times, so we need to 
    # summarize them back down.
    group_by(id) %>% slice(1) %>% ungroup() %>%
    mutate(playground = ifelse(is.na(playground), F, playground)) %>%
    select(id, playground) %>%
    st_set_geometry(NULL)
  
  # determine if the pitch points are inside the park polygon boundaries
  parks_with_pitches <- parks %>%
    select(id) %>%
    st_join(pitches %>% select(sport)) %>%
    st_set_geometry(NULL) %>%
    # multiple pitches in a park result in repeated rows. 
    group_by(id, sport) %>% summarise(count = n()) %>%
    pivot_wider(id_cols = id, values_from = count, names_from = sport, 
                values_fill = FALSE) %>%
    mutate(
      pitch = ifelse(`NA` == 1, FALSE, TRUE),
      across(baseball:tennis, ~ifelse(. > 0, TRUE, FALSE)),
    ) %>%
    select(-`NA`)
  
  # count the number of shops within 100 feet of a park boundary
  parks_with_shops <- parks %>%
    st_buffer(500) %>%
    st_join(shops) %>%
    st_set_geometry(NULL)  %>%
    group_by(id) %>%
    summarise(
      shops = n() - 1
    )
  
  # determine which walking paths are inside park polgon boundaries
  parks_with_trails <- parks %>% 
    # determine if the trails are inside the park polygon boundaries
    st_join(trails %>% select(trail = osm_id), st_intersects) %>%
    # multiple pitches in a park result in repeated rows. 
    group_by(id) %>% slice(1) %>% ungroup() %>%
    mutate(trail = ifelse(is.na(trail), F, T)) %>%
    select(id, trail) %>%
    st_set_geometry(NULL)
  
  # join together and write out =================
  attributed_parks <- parks %>%
    # compute yeo-johnson transformation on acres
    mutate(yj_acres = VGAM::yeo.johnson(acres, lambda = 0)) %>%
    left_join(parks_with_playgrounds, by = "id") %>%
    left_join(parks_with_pitches,     by = "id") %>%
    left_join(parks_with_shops,       by = "id") %>%
    left_join(parks_with_trails,      by = "id") 
}


get_acsdata <- function(){
  variables <- c(
    "population" = "B02001_001", # TOTAL: RACE
    "housing_units" = "B25001_001", # HOUSING UNITS
    "households" = "B19001_001", #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    # Hispanic or Latino Origin by Race
    "white" = "B03002_003",
    "black" = "B03002_004",
    "asian" = "B03002_006",
    "hispanic" = "B03002_012",
    #MEDIAN HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "income" = "B19013_001",
    # FAMILY TYPE BY PRESENCE AND AGE OF RELATED CHILDREN
    "children_c06" = "B11004_004", # married under 6 only
    "children_c6+" = "B11004_005", # married under 6 and older
    "children_m06" = "B11004_011", # male under 6 only
    "children_m6+" = "B11004_012", # male under 6 and older
    "children_f06" = "B11004_017", # female under 6 only
    "children_f6+" = "B11004_018", # female under 6 and older
    #HOUSEHOLD INCOME IN THE PAST 12 MONTHS (IN 2017 INFLATION-ADJUSTED DOLLARS)
    "inc_0010" = "B19001_002",  "inc_1015" = "B19001_003", "inc_1520" = "B19001_004",
    "inc_2025" = "B19001_005", "inc_2530" = "B19001_006", "inc_3035" = "B19001_007",
    "inc_125"  = "B19001_015", "inc_150"  = "B19001_016", "inc_200"  = "B19001_017"
  )
  
  get_acs(geography = "block group", variables = variables,
                 state = "CA", county = "Alameda", geometry = TRUE) %>%
    select(-moe) %>%
    spread(variable, estimate) %>%
    # area is in m^2, change to km^2
    mutate(area = as.numeric(st_area(geometry) * 1e-6)) %>%
    transmute(
      geoid = GEOID,
      group = 1,
      population, households, housing_units, 
      density = households / area,
      income,
      # many of the variables come in raw counts, but we want to consider
      # them as shares of a relevant denominator.
      children = 100 * ( children_c06 + `children_c6+` + 
                           children_m06 + `children_m6+` + 
                           children_f06 + `children_f6+`) / households,
      lowincome    = 100 * (inc_0010 + inc_1015 + inc_1520 + inc_2530 +
                              inc_3035) / households,
      highincome   = 100 * (inc_125 + inc_150 + inc_200) / households,
      black        = 100 * black / population,
      asian        = 100 * asian / population,
      hispanic     = 100 * hispanic / population,
      white        = 100 * white / population
    ) %>%
    filter(population > 0)
  
}

make_street_parks <- function(slowstreets, shops){
  street_parks <- slowstreets %>%
    st_buffer(dist = 25)  %>% # 50 foot wide park ( 4 lanes of traffic)
    mutate(acres = as.numeric(st_area(.)) / 43560) %>% 
    mutate() %>%
    transmute(
      id = str_c("st", ID), access = "Open Access", acres, type = "Street",
      yj_acres = VGAM::yeo.johnson(acres, lambda = 0),
      playground = FALSE, baseball = FALSE, basketball = FALSE, 
      `football / soccer` = FALSE, other_pitch = FALSE, volleyball = FALSE,
      tennis = FALSE, pitch = FALSE, trail = TRUE, attractions = NA
    )
  
  shopped_streets <- street_parks %>%
    st_buffer(500) %>%
    st_join(shops) %>%
    st_set_geometry(NULL)  %>%
    group_by(id) %>%
    summarise(
      shops = n() - 1
    )
  
  street_parks %>%
    left_join(shopped_streets, by = "id")
}


extract_paths <- function(shortest_paths_7z){
  shortest_path_dir  <- "data/shortest_paths"
  if(!dir.exists(shortest_path_dir)) {
    # CTV: This bit doesn't work on my computer (can't extract 7z
    # files from code? So I just extracted them from the 7zip utility
    # within Windows Explorer prior to running)
    system2("7z", args = c("x", shortest_paths_7z, "-odata/"))
  }
  dir(shortest_path_dir, full.names = TRUE)
}

extract_paths_street <- function(shortest_paths_7z){
  # distances computed in py/shortest_paths.py
  shortest_path_dir <- "data/shortest_paths_street"
  if(!dir.exists(shortest_path_dir)) {
    system2("7z", args = c("x", shortest_paths_7z, "-odata/"))
  }
  path_files <- dir(shortest_path_dir, full.names = TRUE)
}

get_shortest_paths <- function(path_files){
  
  lapply(path_files, function(file) {
    read_csv(file, col_types = list(geoid = col_character(), park_id = col_character())) %>%
      mutate(
        yj_distance = VGAM::yeo.johnson(distance, lambda = 0),
        yj_euc_dist = VGAM::yeo.johnson(euc_dist, lambda = 0)
      )
  }) %>%
    bind_rows() %>%
    rename(home = geoid, park = park_id) %>%
    mutate(home = str_pad(home, width = 12, side = "left", pad = "0"))
}


make_logitdata <- function(park_flows, attributed_parks, distance_df, acs, n_obs, n_alts){
  mydata <- park_flows %>%
    ungroup() %>%
    mutate(weight = flow / sum(flow)) %>%
    sample_n(n_obs, replace = TRUE, weight = weight) %>%
    transmute(id = row_number(), home, alt_0 = park, 
              validation = sample(c(TRUE,FALSE), n(), TRUE, prob = c(0.2, 0.8)))
  
  
  sampled_parks <- lapply(1:n_obs, function(i){
    sample(attributed_parks$id, n_alts)
  }) %>%
    unlist() %>%
    matrix(ncol = n_alts) %>%
    as_tibble(.name_repair = ~ str_c("alt", 1:n_alts, sep = "_"))
  
  mydata %>%
    bind_cols(sampled_parks) %>%
    gather(key = "alt", value = "park", -id, -home, -validation) %>%
    mutate(chosen = alt == "alt_0") %>%
    arrange(id, alt) %>%
    
    # append distances
    inner_join(distance_df, by = c("home", "park")) %>%
    
    # append park attributes
    left_join(attributed_parks, by = c("park" = "id")) %>%
    
    # append block group attributes
    left_join(acs %>% select(geoid, density:white) %>% st_set_geometry(NULL),
              by = c("home" = "geoid")
              
    )
}


make_benefits <- function(logsums){
  # percent of households / individuals in each group
  demovars <- c("children", "lowincome", "highincome", "black", 
                "asian", "hispanic", "white", "all")
  demonames <- c("Households with Children under 6", "Income < $35k",
                 "Income > $125k", "Black", "Asian", "Hispanic", "White", 
                 "All Households")
  
  # Raw demographic totals
  pop_dist <- logsums %>% st_set_geometry(NULL) %>% as_tibble()  %>%
    mutate(all = 100) %>%
    mutate_at( demovars, (~ ./100 * households)) %>%
    select(all_of(demovars)) %>%
    summarise_all(~sum(., na.rm = TRUE)) %>%
    gather("group", "population") %>%
    mutate(
      `pct_pop` = population / population[length(demonames)] * 100 
    )
  
  bene_dist <- logsums %>% st_set_geometry(NULL) %>% as_tibble()  %>%
    # benefit is the choice benefit * households * proportion of category 
    mutate(all = 100) %>%
    mutate_at( demovars, (~ ./100 * households * diff_ls)) %>%
    select(all_of(demovars)) %>%
    summarise_all(~sum(., na.rm = TRUE)) %>%
    gather("group", "benefit")    %>% 
    mutate(
      pct_benefit = benefit / benefit[length(demonames)] * 100 
    ) 
  
  left_join( bene_dist, pop_dist, by = "group" ) %>%
    mutate(
      group = demonames,
      group = fct_relevel(group, "All Households", after = 1),
      benefit = scales::dollar(benefit),
      population = scales::comma(population)
    )
}
