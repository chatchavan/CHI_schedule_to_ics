# prepare data form the whole CHI program (needed to grab DOI and broadcast link)
library(tidyverse)
library(lubridate)
library(snakecase)

tz_chi_program_json <- "UTC"
tz_source <- "Asia/Tokyo"

chi_program <- jsonlite::fromJSON("input/CHI_2021_program.json")

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


df_sessions %>% write_rds("data/df_sessions.RDS")
df_contents %>% write_rds("data/df_contents.RDS")