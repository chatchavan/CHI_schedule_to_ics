library(tidyverse)
library(lubridate)
library(snakecase)
library(calendar)

tz_source <- "Asia/Tokyo"
ical_description_template <- 
  "DOI: {doi}
  Session name: {session_name}
  ==============
  {my_note}
  "


# process the exported schedule
tidy_schedule <- function(path_schedule_csv, tz_target) {
  
  df_sessions <- read_rds("data/df_sessions.RDS")
  df_contents <- read_rds("data/df_contents.RDS")
  
  df_chi_export <- 
    read_csv(path_schedule_csv, col_types = "cccc___c") %>% 
    rename_with(to_snake_case) %>% 
    filter(!is.na(start_date)) %>%    # remove the "reading list" bookmarks
    mutate(across(ends_with("_date"), ~ymd_hm(.x, tz = tz_source))) 
  
  # 1. match with the whole CHI content by title
  df_by_content <-
    df_chi_export %>% 
    filter(!is.na(content_title)) %>%
    left_join(df_contents, by = "content_title", suffix = c("", ".y"))
  
  # 2. match with the whole CHI content by session name (when title doesn't exist)
  df_by_session <-
    df_chi_export %>% 
    filter(is.na(content_title)) %>% 
    left_join(df_sessions, by = "session_name", suffix = c("", ".y")) %>% 
    select(-content_ids)
  
  # combine 1. and 2. and filter duplicates due to session-repeats (specific to CHI 2021)
  df_schedule_src <- 
    bind_rows(df_by_content, df_by_session) %>% 
    filter(abs(start_date - start_date.y) < 360)  %>% # remove the entries that has the start time differs more than 360 seconds
    select(-ends_with(".y"))
  
  # convert to the target timezone
  df_target_tz <-
    df_schedule_src %>% 
    mutate(across(ends_with("_date"), ~with_tz(.x, tz_target), .names = "{.col}_target"))
  
}

# write the processed schedule to ics
write_ics <- function(chi_df, path_output) {
  df_ics <- 
    chi_df %>% 
    mutate(
      ical_title = if_else(!is.na(content_title), content_title, session_name),
      ical_url = broadcast_link,
      ical_description = str_glue(ical_description_template, .na = "-")) %>%
    select(start_date_target, end_date_target, ical_title, ical_url, ical_description) %>% 
    
    # create ical object
    mutate(ical_obj = pmap(.,function(start_date_target, end_date_target, ical_title, ical_url, ical_description){
      ical_obj <- ic_event(start_time = start_date_target,
                           end_time = end_date_target,
                           summary = ical_title)
      ical_obj$DESCRIPTION <- str_replace_all(ical_description, fixed("\n"), fixed("\\n"))
      ical_obj$URL <- ical_url
      ical_obj
    }))
  
  # generate ics text format
  df_ics %>% 
    pull(ical_obj) %>% 
    bind_rows() %>% 
    ic_character() %>% 
    str_flatten("\n") %>% 
    write_file(path_output)
}