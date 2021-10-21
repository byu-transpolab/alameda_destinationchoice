# function to index a tibble for mnl estimation
logit_converter <- function(df) {
  dfidx(df, idx = list("id", "alt"), shape = "long",  
        choice = "chosen", idnames = "id", drop.index = FALSE)
}

# function to estimate the mnl model on a subset of data
group_mnl <- function(df){
  tryCatch({
    mlogit(chosen ~ yj_distance + yj_acres + playground + baseball +  basketball +
             `football / soccer` + other_pitch + volleyball + tennis + trail | -1,
           data = df)
  }, error = function(e){
    warning(e, "could not estimate mnl")
    return(NA)
  })
}


make_estim <- function(logitdata){
  dfi <- logit_converter(logitdata)
  valid <- filter(dfi, validation)
  dfi
}

estimate_base_models <- function(estim){
  
  list(
    "Network Distance"   = mlogit(
      chosen ~ yj_distance + yj_acres | -1 ,  data = estim),
    "Park Attributes"    = mlogit(
      chosen ~ yj_distance + yj_acres +  playground + trail + pitch | -1 , 
      data = estim),
    "Sport Detail"    = mlogit(
      chosen ~ yj_distance + yj_acres + playground + baseball +  basketball +
        `football / soccer` + other_pitch + volleyball + tennis + trail | -1 , 
      data = estim)
  )
}


estimate_grouped_models <- function(estim){
  minority_models <- estim %>%
    mutate(group = case_when( black > 30 ~ "black", asian > 30 ~ "asian", 
                              hispanic > 30 ~ "hispanic", TRUE ~ "othermin"
    )) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>% nest() %>% ungroup() %>%
    mutate(
      data = map(data, logit_converter),
      mnl = map(data, group_mnl)
    ) 
  income_models <- estim %>%
    mutate(group = case_when(lowincome > 30 ~ "lowincome", 
                             highincome > 50 ~ "highincome", TRUE ~ "otherinc")) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>% nest() %>% ungroup() %>%
    mutate(
      data = map(data, logit_converter),
      mnl = map(data, group_mnl)
    )
  child_models <- estim %>%
    mutate(group = case_when(children > 25 ~ "children", 
                             children < 5 ~ "few children", TRUE ~ "otherch")) %>%
    filter(!is.na(group)) %>%
    group_by(group) %>% nest() %>% ungroup() %>%
    mutate(
      data = map(data, logit_converter),
      mnl = map(data, group_mnl)
    )
  
  lapply(list(child_models, income_models, minority_models), 
                           function(df){
                             lapply(df$mnl, function(x) x) %>% 
                               set_names(nm = c(df$group)) 
                           }) %>%
    unlist(recursive = FALSE)
}



# write a function to estimate multiple MNL models on different 
# segments of the estimation data
estimate_split_models <- function(data, var, splits) {
  #print(var)
  lapply(splits, function(p){
    #print(p)
    data %>% 
      filter(.data[[var]] > p) %>% tibble() %>%
      mutate(share = p)
  }) %>%
    bind_rows() %>% group_by(share) %>% nest() %>%
    mutate(
      data = map(data, logit_converter),
      mnl = map(data, group_mnl),
      coef = map(mnl, tidy)
    )
}


estimate_all_splits <- function(estim){
  splits <- c(0, 5, 10, 15, 20, 30, 40)
  lapply(c("black", "asian", "hispanic", "lowincome", "highincome", "children"), function(var) {
    estimate_split_models(estim, var, splits) %>%
      mutate(Segmentation = var)
  }) %>% bind_rows() 
}




make_split_dat <- function(split_models){
  # extract the coefficients you need to plot
  modelplot(split_models$mnl %>% 
                     setNames(str_c(split_models$Segmentation, split_models$share, sep = "_")),
                   coef_map = this_map, draw = FALSE) %>%
    separate(model, into = c("Segmentation", "share"), convert = TRUE, sep = "_") %>%
    mutate(
      Segmentation = case_when(
        Segmentation == "black" ~ "Black",
        Segmentation == "asian" ~ "Asian",
        Segmentation == "hispanic" ~ "Hispanic",
        Segmentation == "lowincome" ~ "Income < $35k",
        Segmentation == "highincome" ~ "Income > $125k",
        Segmentation == "children" ~ "Children under 6"
      )
    )
  
}



make_logsums <- function(distance_df, attributed_parks, street_distances, street_parks, 
                         base_models, acs){
  
  attributed_parks %<>% mutate(chosen = FALSE) %>% ungroup()
  attributed_parks$chosen[55] <- TRUE
  
  # create base situation prediction dataset
  base_choices <- distance_df %>%
    left_join(attributed_parks, by = c("park" = "id")) %>% 
    rename(id = home, alt = park) %>%
    logit_converter()
  
  # create streets situation prediction dataset, augmented by street parks
  street_choices <-  left_join(
    bind_rows(street_distances, distance_df),
    bind_rows(street_parks %>% mutate(chosen = FALSE, attractions = FALSE), 
              attributed_parks),
    by = c("park" = "id")) %>%
    rename(id = home, alt = park) %>%
    logit_converter()
  
  
  logsums <- lapply(list("Base" = base_choices, "Streets" = street_choices), function(df){
    u <- predict(base_models$`Sport Detail`, newdata = df, type = "linpred")
    logsum <- log(rowSums(exp(u)))
    list( u = u, logsum = logsum )
  })
  
  logsums_tibble <- tibble(
    geoid = names(logsums$Base$logsum),
    base = logsums$Base$logsum,
    streets = logsums$Streets$logsum
  )
  
  acs %>%
    left_join(logsums_tibble, by = "geoid") %>%
    mutate(diff_ls = (streets - base) / 0.215)
}




















