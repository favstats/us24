source("utils.R")
# ?get_targeting
# get_targeting("41459763029", timeframe = "LAST_90_DAYS")
# debugonce(get_targeting)

library(httr)
library(tidyverse)
library(lubridate)
library(httr2)


get_page_insights <- function (pageid, timeframe = "LAST_30_DAYS", lang = "en-GB", 
          iso2c = "US", include_info = c("page_info", "targeting_info"), 
          join_info = T) 
{
  ua_list <- c("Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.3", 
               "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/89.0.4389.82 Safari/537.36", 
               "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/91.0.4472.77 Safari/537.36")
  ua <- sample(ua_list, 1)
  fetch_page_info <- ifelse("page_info" %in% include_info, 
                            "true", "false")
  resp <- httr2::request("https://www.facebook.com/api/graphql/") %>% 
    httr2::req_headers(`Accept-Language` = paste0(lang, 
                                                  ",", stringr::str_split(lang, "-") %>% unlist() %>% 
                                                    .[1], ";q=0.5"), `sec-fetch-site` = "same-origin", 
                       `user-agent` = ua) %>% httr2::req_body_raw(glue::glue("av=0&_aaid=0&user=0&a=1&req=3&hs=19797.BP%3ADEFAULT.2.0..0.0&dpr=1&ccg=EXCELLENT&rev=1012093869&s=sbbnic%3Awquopy%3A7r1j3c&hsi=7346737420686302672&dyn=7xe6Eiw_K9zo5ObwKBAgc9o2exu13wqojyUW3qi4EoxW4E7SewXwCwfW7oqx60Vo1upEK12wvk1bwbG78b87C2m3K2y11wBw5Zx62G3i1ywdl0Fw4Hwp8kwyx2cU8EmwoHwrUcUjwVw9O7bK2S2W2K4EG1Mxu16wciaw4JwJwSyES0gq0K-1LwqobU2cwmo6O1Fw44wt8&csr=&lsd=AVo6-wl7l1Q&jazoest=2881&spin_r=1012093869&spin_b=trunk&spin_t=1710545602&_jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryMobileFocusedStateProviderQuery&variables=%7B%22adType%22%3A%22POLITICAL_AND_ISSUE_ADS%22%2C%22audienceTimeframe%22%3A%22{timeframe}%22%2C%22country%22%3A%22{iso2c}%22%2C%22viewAllPageID%22%3A%22{pageid}%22%2C%22fetchPageInfo%22%3A{fetch_page_info}%2C%22fetchSharedDisclaimers%22%3Atrue%2C%22active_status%22%3A%22ALL%22%2C%22ad_type%22%3A%22POLITICAL_AND_ISSUE_ADS%22%2C%22bylines%22%3A%5B%5D%2C%22collation_token%22%3A%227ca3912f-0148-43ce-83e4-9a68ef656e4d%22%2C%22content_languages%22%3A%5B%5D%2C%22count%22%3A30%2C%22countries%22%3A%5B%22{iso2c}%22%5D%2C%22excluded_ids%22%3A%5B%5D%2C%22full_text_search_field%22%3A%22ALL%22%2C%22group_by_modes%22%3A%5B%5D%2C%22image_id%22%3Anull%2C%22location%22%3Anull%2C%22media_type%22%3A%22ALL%22%2C%22page_ids%22%3A%5B%5D%2C%22pagination_mode%22%3Anull%2C%22potential_reach_input%22%3Anull%2C%22publisher_platforms%22%3A%5B%5D%2C%22query_string%22%3A%22%22%2C%22regions%22%3A%5B%5D%2C%22search_type%22%3A%22PAGE%22%2C%22session_id%22%3A%221678877b-700b-485a-abb0-60efcb6b4019%22%2C%22sort_data%22%3A%7B%22mode%22%3A%22SORT_BY_RELEVANCY_MONTHLY_GROUPED%22%2C%22direction%22%3A%22ASCENDING%22%7D%2C%22source%22%3Anull%2C%22start_date%22%3Anull%2C%22view_all_page_id%22%3A%22{pageid}%22%7D&server_timestamps=true&doc_id=7193625857423421"), 
                                                                  "application/x-www-form-urlencoded") %>% httr2::req_perform()
  out <- resp %>% httr2::resp_body_html() %>% rvest::html_element("p") %>% 
    rvest::html_text() %>% str_split_1("(?<=\\})\\s*(?=\\{)") %>% 
    map(jsonlite::fromJSON)
  if (!is.null(out[[1]][["errors"]][["description"]])) {
    message(out[[1]][["errors"]][["description"]])
  }
  if ("page_info" %in% include_info) {
    page_info1 <- out[[1]][["data"]][["ad_library_page_info"]][["page_info"]]
    if (is.null(page_info1)) {
      if ("page_info" %in% include_info & "targeting_info" %in% 
          include_info) {
        if (join_info) {
          return(tibble(page_id = pageid, no_data = T))
        }
        else {
          return(list(page_info = tibble(page_id = pageid, 
                                         no_data = T), targeting_info = tibble(page_id = pageid, 
                                                                               no_data = T)))
        }
      }
      else {
        return(tibble(page_id = pageid, no_data = T))
      }
    }
    my_dataframe <- as.data.frame(t(unlist(page_info1)), 
                                  stringsAsFactors = FALSE) %>% dplyr::mutate_all(as.character)
    page_info2_raw <- out[[2]][["data"]][["page"]][["shared_disclaimer_info"]][["shared_disclaimer_pages"]][["page_info"]]
    if (!is.null(page_info2_raw)) {
      page_info2 <- page_info2_raw %>% tibble::as_tibble() %>% 
        dplyr::mutate_all(as.character) %>% dplyr::mutate(shared_disclaimer_info = my_dataframe$page_id[1])
    }
    else {
      page_info2 <- tibble(no_shared_disclaimer = T)
    }
    creat_times <- out[[1]][["data"]][["page"]][["pages_transparency_info"]][["history_items"]] %>% 
      dplyr::mutate(event = paste0(item_type, ": ", as.POSIXct(event_time, 
                                                               origin = "1970-01-01", tz = "UTC"))) %>% dplyr::select(event) %>% 
      unlist() %>% t() %>% as.data.frame()
    about_text <- out[[1]][["data"]][["page"]][["about"]] %>% 
      purrr::set_names("about")
    address_raw <- out[[1]][["data"]][["page"]][["confirmed_page_owner"]][["information"]]
    if (!is.null(address_raw)) {
      address <- address_raw %>% purrr::flatten()
    }
    else {
      address <- tibble(no_address = T)
    }
    sdis_raw <- out[[2]][["data"]][["page"]][["shared_disclaimer_info"]][["shared_disclaimer_pages"]][["page_info"]]
    if (!is.null(sdis_raw)) {
      sdis <- sdis_raw %>% dplyr::mutate_all(as.character) %>% 
        dplyr::mutate(shared_disclaimer_page_id = pageid[1]) %>% 
        jsonlite::toJSON() %>% as.character()
    }
    else {
      sdis <- "[]"
    }
    page_info <- my_dataframe %>% dplyr::mutate(shared_disclaimer_info = sdis) %>% 
      dplyr::bind_cols(about_text) %>% dplyr::bind_cols(creat_times) %>% 
      dplyr::bind_cols(address)
  }
  if ("targeting_info" %in% include_info) {
    out_raw <- out[[1]][["data"]][["page"]][["ad_library_page_targeting_insight"]]
    summary_dat <- out_raw %>% purrr::pluck("ad_library_page_targeting_summary") %>% 
      dplyr::bind_rows()
    if (nrow(summary_dat) > 1) {
      summary_dat <- summary_dat %>% dplyr::slice(which(summary_dat$detailed_spend$currency == 
                                                          summary_dat$main_currency)) %>% dplyr::select(-detailed_spend)
    }
    targeting_details_raw <- out_raw[!(names(out_raw) %in% 
                                         c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
    targeting_info <- targeting_details_raw %>% purrr::discard(purrr::is_empty) %>% 
      purrr::imap_dfr(~{
        .x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))
      }) %>% dplyr::bind_cols(summary_dat) %>% dplyr::mutate(page_id = pageid)
  }
  if ("page_info" %in% include_info & "targeting_info" %in% 
      include_info) {
    if (join_info) {
      fin <- page_info %>% left_join(targeting_info, by = "page_id")
    }
    else {
      fin <- list(page_info, targeting_info)
    }
  }
  else if ("page_info" %in% include_info) {
    return(page_info)
  }
  else if ("targeting_info" %in% include_info) {
    return(targeting_info)
  }
  return(fin)
}


# installed <- installed.packages() %>% 
#   as_tibble() %>% 
#   filter(Package == "metatargetr") %>% 
#   nrow()
# 
# if(installed == 0){
#   remotes::install_github("favstats/metatargetr", force = T)
# }
# 
# library(metatargetr)

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
