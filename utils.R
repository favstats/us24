calc_targeting <- function(only_tags) {

    total_sppppeen <- only_tags %>%
        distinct(internal_id, .keep_all = T)  %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        select(internal_id, total_spend) %>%
        arrange(desc(total_spend)) %>%
        summarize(total_spend = sum(total_spend))

    howmuchisinterest <- only_tags %>%
        filter(type == "detailed") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        arrange(desc(spend_per)) %>%
        summarize(spend_per = sum(spend_per)) %>%
        mutate(target = "interest")

    howmuchislocation <- only_tags %>%
        filter(type == "location") %>%
        group_by(internal_id, location_type) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per, location_type) %>%
        arrange(desc(spend_per)) %>%
        group_by(location_type) %>%
        summarize(spend_per = sum(spend_per)) %>%
        rename(target = location_type)

    howmuchisage <- only_tags %>%
        filter(type == "age") %>%
        filter(total_spend_pct != 0) %>%
        group_by(internal_id) %>%
        mutate(n_ages = n()) %>% #count(n_ages, sort = T)
        ungroup() %>%
        filter(n_ages <= 47) %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "age")

    howmuchisgender <- only_tags %>%
        filter(type == "gender") %>%
        filter(total_spend_pct != 0) %>%
        filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "gender")

    howmuchcustom <- only_tags %>%
        filter(type == "custom_audience") %>%
        filter(total_spend_pct != 0) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "custom_audience")


    howmuchlookalike <- only_tags %>%
        filter(type == "lookalike_audience") %>%
        filter(total_spend_pct != 0) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "lookalike_audience")

    howmuchlanguage <- only_tags %>%
        filter(type == "language") %>%
        filter(total_spend_pct != 0) %>%
        drop_na(value) %>%
        # filter(value != "All") %>%
        group_by(internal_id) %>%
        filter(total_spend_pct == max(total_spend_pct)) %>%
        slice(1) %>%
        ungroup() %>%
        # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
        mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
        mutate(spend_per = total_spend * total_spend_pct) %>%
        select(internal_id, spend_per) %>%
        summarize(spend_per = sum(spend_per))  %>%
        mutate(target = "language")

    targeting_on_each <- howmuchisinterest %>%
        bind_rows(howmuchislocation) %>%
        bind_rows(howmuchisage) %>%
        bind_rows(howmuchisgender) %>%
        bind_rows(howmuchcustom) %>%
        bind_rows(howmuchlookalike) %>%
        bind_rows(howmuchlanguage) %>%
        mutate(total = total_sppppeen$total_spend) %>%
        mutate(perc = spend_per/total*100) %>%
        arrange(desc(perc))

    return(targeting_on_each)
}

relationshipstuff <- "Widowed|Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified"


add_ribbons <- function(x, adv, col) {
    x %>%
        tab_options(table.width = pct(100)) %>%
        tab_style(
            style = cell_borders(
                sides = c("left"),
                color = col,
                weight = px(18.5),
                style = "solid"
            ),
            locations = cells_body(
                columns = `Number of Advertisers`,
                rows = adv
            ))
}




append_date_suffix <- function(dates){
  dayy <- lubridate::day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

create_date <- function(x) {
  the_date <- format(x, "%b %d")
  the_date <- ifelse(str_detect(the_date, " 0"),
                     str_remove(the_date, "0"),
                     the_date)
  str_replace(the_date, 
              as.character(lubridate::day(x)), 
              append_date_suffix(x))
}


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}

map_chr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_chr(.x, f, ...)
}

prepp <- function(tf) {
  
  unlink(paste0("targeting/", tf), recursive = T, force = T)
  
  dir.create(paste0("targeting/", tf))
  
  write_lines("_", paste0("targeting/", tf, "/", "_"))
  
  # }
  
}


combine_em <- function(da30) {
  
  tf <- da30 %>% arrange(tframe) %>% slice(1) %>% pull(tframe)
  
  minimum_date <- dir("historic", recursive = T) %>%
    keep(~str_detect(.x, paste0(tf, "\\.rds"))) %>% 
    str_remove("/.*") %>%
    as.Date() %>%
    min(na.rm = T)
  
  latest_ds <- da30 %>% arrange(ds) %>% slice(1) %>% pull(ds) %>% as.Date()
  
  begintf <- as.Date(latest_ds) - lubridate::days(tf)
  
  date_vector <- vector()
  current_date <- latest_ds
  index <- 1
  
  while(current_date > minimum_date) {
    
    date_vector[index] <- current_date
    
    current_date <- current_date - lubridate::days(tf)
    
    index <- index + 1
    
  }
  
  if(length(date_vector != 0)){
    
    
    combined_dat <- paste0("historic/", as_date(date_vector), "/", tf, ".rds") %>%
      map_dfr(~{
        if(!file.exists(.x)){
          return(tibble(ds = as.character(begintf), missing_report = T))
        } else {
          readRDS(.x)
        }
        
      })
    
    saveRDS(combined_dat, file = paste0("data/combined_dat", tf,  ".rds"))
    
    aggr <- combined_dat  %>%
      mutate(total_spend = readr::parse_number(as.character(total_spend_formatted))) %>%
      mutate(total_spend = ifelse(total_spend == 50, 50, total_spend)) %>%
      mutate(total_spend = total_spend * total_spend_pct) %>%
      group_by(page_id, value, type, location_type, detailed_type, custom_audience_type, is_exclusion) %>%
      summarize(total_spend = sum(total_spend),
                num_ads = sum(num_ads),
                num_obfuscated = sum(num_obfuscated)) %>%
      ungroup()
    
    saveRDS(aggr, file = paste0("data/election_dat_aggr", tf,  ".rds"))
    
    
    
  }
  
  
  # if(new_ds == latest_ds){
  
  unlink(paste0("targeting/", tf), recursive = T, force = T)
  
  dir.create(paste0("targeting/", tf))
  
  write_lines("_", paste0("targeting/", tf, "/", "_"))
  
  # }
  
}

