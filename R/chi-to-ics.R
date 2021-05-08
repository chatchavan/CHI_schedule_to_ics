# convert data exported from the CHI program to iCal format (.ics) to be imported to calandar apps
#   by Chat Wacharamanotham
#   Tested with https://programs.sigchi.org/chi/2021/ version 3.3.7

library(tidyverse)
library(lubridate)
library(snakecase)
library(calendar)

# input files
path_schedule_csv <- "input/CHI2021_my_schedule.csv"
path_program_json <- "input/CHI_2021_program.json"

# exclusion
exclude_before_ymd <- "2021-05-08"

# time zones
tz_chi_program_json <- "UTC"
tz_source <- "Asia/Tokyo"
tz_target <- "Europe/Zurich"

# template for the description field in the output .ics
ical_description_template <- 
  "DOI: {doi}
  Session name: {session_name}
  ==============
  {my_note}
  "

#===============================================================================
# prepare data form the whole CHI program (needed to grab DOI and broadcast link)

chi_program <- jsonlite::fromJSON(path_program_json)

df_timeslots <- 
  as_tibble(chi_program$timeSlots) %>% 
  rename_with(to_snake_case) %>% 
  mutate(across(ends_with("_date"), ~(.x+0.1) / 1000)) %>% 
  mutate(across(ends_with("_date"), ~as_datetime(.x, tz = tz_chi_program_json))) %>%
  mutate(across(ends_with("_date"), ~force_tz(.x, tz_source)))

df_sessions <-
  as_tibble(chi_program$sessions) %>% 
  rename_with(to_snake_case) %>% 
  select(session_name = name, broadcast_link, time_slot_id, content_ids) %>%
  mutate(broadcast_link = .$broadcast_link$url) %>% 
  left_join(df_timeslots, by = c("time_slot_id" = "id")) %>% 
  unnest(content_ids, keep_empty = TRUE) %>% 
  select(-type, -time_slot_id)
  

df_contents <-
  as_tibble(chi_program$contents) %>% 
  rename_with(to_snake_case) %>%
  select(id, content_title = title, doi, abstract, videos) %>% 
  left_join(df_sessions, by = c("id" = "content_ids")) %>% 
  select(content_title, session_name, start_date, end_date, broadcast_link, doi)


#===============================================================================
# process the exporteds schedule

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
  
  
  
df_target_tz %>%   
  write_ics("output/all_schedule.ics")

df_target_tz %>%   
  filter(start_date_target > ymd(exclude_before_ymd)) %>% 
  write_ics("output/filtered_schedule.ics")

df_target_tz %>%   
  mutate(start_ymd = str_c(year(start_date_target), month(start_date_target), day(start_date_target), sep = "-")) %>%
  nest(schedule = !start_ymd) %>% 
  pwalk(function(start_ymd , schedule) {
    path_ics <- file.path("output", sprintf("%s.ics", start_ymd))
    schedule %>% 
      write_ics(path_ics)
  })
