source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)
library(lubridate)
library(metatargetr)

installed <- installed.packages() %>% 
  as_tibble() %>% 
  filter(Package == "metatargetr") %>% 
  nrow()

if(installed == 0){
  remotes::install_github("favstats/metatargetr", force = T)
}

if(Sys.info()[["sysname"]]=="Windows"){
  ### CHANGE ME WHEN LOCAL!
  tf <- "30"
}

jb <- get_page_insights("7860876103", timeframe = glue::glue("LAST_90_DAYS"), include_info = "targeting_info") %>% as_tibble()

new_ds <- jb %>% arrange(ds) %>% slice(1) %>% pull(ds)

latest_elex <- readRDS(paste0("data/election_dat", 30, ".rds"))

latest_ds <- latest_elex %>% arrange(ds) %>% slice(1) %>% pull(ds)






tstamp <- Sys.time()

write_lines(lubridate::as_date(tstamp), "tstamp.txt")

dir.create(paste0("historic/", new_ds), recursive = T)

c(7, 30, 90) %>%
  walk(prepp)

wtm_data <-
  openxlsx::read.xlsx("data/Presidential candidates, last 30 days.xlsx", sheet = 2) %>% janitor::clean_names()

uswtm <- read_csv("data/1dd84cee-7d36-43f7-a6fb-f71e4fbd8040.csv.gzip") 
  # count(enti)

  wtm_data <-
    uswtm %>% 
  filter(entities.short_name %in% c("Harris", "Trump", "Dems", "DemPAC", "RepPAC", "Prog", "Con", "GOP")) %>%
  distinct(advertisers_platforms.advertiser_platform_ref, .keep_all = T) %>% 
  mutate(party = case_when(
    entities.short_name == "Dems" ~ "Democrats",
    entities.short_name == "GOP" ~ "Republicans",
    entities.short_name == "Harris" ~ "Kamala Harris",
    entities.short_name == "Trump" ~ "Donald Trump",
    T ~ entities.name
   )) %>% 
      mutate(affiliation = case_when(
        entities.short_name == "Harris" ~ "Democratic",
        entities.short_name == "Trump" ~ "Republican",
        T ~ "Other"
      )) %>% 
  mutate(page_id = advertisers_platforms.advertiser_platform_ref) %>% 
  # distinct(entities.color) %>% 
  mutate(color = entities.color) %>% 
  filter(platforms.name == "Facebook") %>% 
      mutate(page_name = name)
  # View()
  
  # uswtm %>% count(entities.short_name, sort = T) %>% View()
  
all_dat <- wtm_data %>%
  # bind_rows(more_data) %>%
  distinct(page_id, .keep_all = T) %>%
  add_count(page_name, sort  = T) %>%
  mutate(remove_em = n >= 2 & str_ends(page_id, "0")) %>%
  filter(!remove_em) %>%
  # filter(n >= 2) %>%
  # filter(n >= 2 & str_ends(page_id, "0", negate = T)) %>%
  select(-n)

# all_dat %>% count(party, sort = T)
# all_dat %>% nrow

write_lines(all_dat %>% count(page_id, sort = T) %>% nrow, "n_advertisers.txt")



scraper <- function(.x, time = "7") {
  # print(paste0(.x$page_name,": ", round(which(internal_page_ids$page_id == .x$page_id)/nrow(internal_page_ids)*100, 2)))
  
  yo <-
    get_page_insights(.x$page_id, timeframe = glue::glue("LAST_90_DAYS"), include_info = "targeting_info") %>% 
    mutate(tstamp = tstamp)
  
  if (nrow(yo) != 0) {
    path <- paste0(glue::glue("targeting/{time}/"), .x$page_id, ".rds")
    # if(file.exists(path)){
    #   ol <- read_rds(path)
    #
    #   saveRDS(yo %>% bind_rows(ol), file = path)
    # } else {
    
    saveRDS(yo, file = path)
    # }
  }
  
  # print(nrow(yo))
  # })
  
}

scraper <- possibly(scraper, otherwise = NULL, quiet = F)


# if(F){
#     # dir("targeting/7", full.names
# }
# da30 <- readRDS("data/election_dat30.rds")
# da7 <- readRDS("data/election_dat7.rds")

### save seperately
all_dat %>%
  split(1:nrow(.)) %>%
  walk_progress(scraper, 7)

all_dat %>%
  split(1:nrow(.)) %>%
  walk_progress(scraper, 30)

all_dat %>%
  split(1:nrow(.)) %>%
  walk_progress(scraper, 90)

da30  <- dir("targeting/30", full.names = T) %>%
  discard( ~ str_detect(.x, "/_")) %>%
  map_dfr_progress(readRDS)  %>%
  mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
  # rename(page_id = internal_id) %>%
  left_join(all_dat) %>% 
  mutate(tframe = "30")

da7  <- dir("targeting/7", full.names = T) %>%
  discard( ~ str_detect(.x, "/_")) %>%
  map_dfr_progress(readRDS) %>%
  mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
  # rename(page_id = internal_id) %>%
  left_join(all_dat) %>% 
  mutate(tframe = "7")

da90  <- dir("targeting/90", full.names = T) %>%
  discard( ~ str_detect(.x, "/_")) %>%
  map_dfr_progress(readRDS) %>%
  mutate(total_spend_formatted = parse_number(total_spend_formatted)) %>%
  # rename(page_id = internal_id) %>%
  left_join(all_dat) %>% 
  mutate(tframe = "90")

saveRDS(da90, "data/election_dat90.rds")
saveRDS(da30, "data/election_dat30.rds")
saveRDS(da7, "data/election_dat7.rds")

saveRDS(da90, paste0("historic/", new_ds, "/90.rds"))
saveRDS(da30, paste0("historic/", new_ds, "/30.rds"))
saveRDS(da7, paste0("historic/", new_ds, "/7.rds"))

list(da7, da30, da90) %>%
  walk(combine_em)
