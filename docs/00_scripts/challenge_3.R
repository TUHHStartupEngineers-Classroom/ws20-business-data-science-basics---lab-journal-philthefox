
# 1. Patent Dominance: What US company / corporation has the most patents? List
#    the 10 US companies with the most assigned/granted patents.

# 2. Recent patent activity: What US company had the most patents granted in
#    2019? List the top 10 companies with the most new granted patents for 2019.

# 3. Innovation in Tech: What is the most innovative tech sector? For the top 10
#    companies (worldwide) with the most patents, what are the top 5 USPTO tech
#    main classes?

# Question	Table
# 1         assignee, patent_assignee
# 2	        assignee, patent_assignee, patent
# 3	        assignee, patent_assignee, uspc

# 1.0 LIBRARIES ----

library(tidyverse)
library(vroom)
library(lubridate)

# 2.0 DATA IMPORT ----

# 2.1 assignee ----

col_types <- list(
  id = col_character(),
  type = col_skip(),
  name_first = col_skip(),
  name_last = col_skip(),
  organization = col_character()
)

assignee_tbl <- vroom(
  file       = "00_data/patent_data/assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 2.2 patent ----

col_types <- list(
  id = col_character(),
  type = col_character(),
  number = col_skip(),
  country = col_skip(),
  date = col_date("%Y-%m-%d"),
  abstract = col_skip(),
  title = col_skip(),
  kind = col_skip(),
  num_claims = col_skip(),
  filename = col_skip(),
  withdrawn = col_skip()
)

patent_tbl <- vroom(
  file       = "00_data/patent_data/patent.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 2.3 patent_assignee ----

col_types <- list(
  patent_id = col_character(),
  assignee_id = col_character(),
  location_id = col_character()
)

patent_assignee_tbl <- vroom(
  file       = "00_data/patent_data/patent_assignee.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 2.4 uspc ----

col_types <- list(
  uuid = col_skip(),
  patent_id = col_character(),
  mainclass_id = col_character(),
  subclass_id = col_skip(),
  sequence = col_skip()
)

uspc_tbl <- vroom(
  file       = "00_data/patent_data/uspc.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 2.5 location ----

col_types <- list(
  id = col_character(),
  city = col_skip(),
  state = col_skip(),
  country = col_character(),
  latitude = col_skip(),
  longitude = col_skip(),
  county = col_skip(),
  state_fips = col_skip(),
  county_fips = col_skip()
)

location_tbl <- vroom(
  file       = "00_data/patent_data/location.tsv", 
  delim      = "\t", 
  col_types  = col_types,
  na         = c("", "NA", "NULL")
)

# 3.0 DATA WRANGLING

# Joining tibbles

patent_data_tbl <- patent_tbl %>% 
  left_join(patent_assignee_tbl, by = c("id" = "patent_id")) %>% 
  left_join(assignee_tbl, by = c("assignee_id" = "id")) %>% 
  left_join(location_tbl, by = c("location_id" = "id")) %>% 
  mutate(year = year(date)) %>% 
  select(id, type, year, organization, country) %>% #, mainclass_id) %>% 
  distinct()

# 4.0 FINDINGS

# 4.1 Patent dominance

patent_dominance_us_tbl <- patent_data_tbl %>% 
  # Filter out non-US companies
  filter(country == "US") %>% 
  # Filter out NAs
  filter(!is.na(organization)) %>%
  # Count patents per organization
  count(organization, sort = TRUE) %>% 
  # Rename the column with numbers of patents
  rename(number_patents = n)

head(patent_dominance_us_tbl, 10)

# 4.2 Recent patent activity

patent_dominance_us_2019_tbl <- patent_data_tbl %>% 
  # Filter out non-US companies
  filter(country == "US") %>% 
  # Filter out NAs
  filter(!is.na(organization)) %>%
  # Filter data for 2019
  filter(year == 2019) %>% 
  # Count patents per organization
  count(organization, sort = TRUE) %>% 
  # Rename the column with numbers of patents
  rename(number_patents = n)

head(patent_dominance_us_2019_tbl, 10)

# 4.3 Innovation in Tech

innovation_main_classes_tbl <- patent_data_tbl %>% 
  # Filter out NAs
  filter(!is.na(organization)) %>%
  # Count patents per organization
  count(organization, sort = TRUE) %>% 
  # Rename the column with numbers of patents
  rename(number_patents = n) %>% 
  # Get the top 10 companies
  top_n(10) %>% 
  # Join with patent data to get patent ids
  left_join(patent_data_tbl) %>% 
  # Join with uspc data
  left_join(uspc_tbl, by = c("id" = "patent_id")) %>% 
  # Count main classes
  count(mainclass_id, sort = TRUE) %>% 
  # Filter out NAs
  filter(!is.na(mainclass_id)) %>%
  # Rename the column with numbers of patents
  rename(number_patents = n)

head(innovation_main_classes_tbl, 5)




