
# 1.0 LIBRARIES ----

library(tidyverse)
library(lubridate)
library(maps)

# 2.0 DATA IMPORT ----

covid_data_tbl <- 
  read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# 3.0 DATA WRANGLING

covid_data_wrangled_tbl <- covid_data_tbl %>% 
  mutate(date = dmy(dateRep)) %>% 
  select(date, cases, deaths, countriesAndTerritories, countryterritoryCode, popData2019, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`) %>% 
  rename(cumulative_14_days_per_100000 = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`) %>% 
  arrange(date)

mortality_rate_tbl <- covid_data_wrangled_tbl %>% 
  group_by(countriesAndTerritories) %>% 
  summarize(deaths_total = sum(deaths)) %>% 
  left_join(covid_data_wrangled_tbl) %>% 
  select(countriesAndTerritories, deaths_total, popData2019) %>% 
  distinct() %>% 
  mutate(mortality_rate = deaths_total / popData2019 * 100) %>% 
  mutate(across(countriesAndTerritories, str_replace_all, "_", " ")) %>%
  mutate(countriesAndTerritories = case_when(
    countriesAndTerritories == "United Kingdom" ~ "UK",
    countriesAndTerritories == "United States of America" ~ "USA",
    countriesAndTerritories == "Czechia" ~ "Czech Republic",
    TRUE ~ countriesAndTerritories))

# 4.0 PLOT

# 4.1 Cumulative Covid-19 cases

covid_data_germany_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "Germany") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_uk_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "United_Kingdom") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_france_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "France") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_spain_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "Spain") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_usa_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "United_States_of_America") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_germany_tbl %>% 
  ggplot(aes(date, cases_total)) +
  geom_line(size = 1) +
  geom_line(data = covid_data_uk_tbl, aes(date, cases_total)) +
  geom_line(data = covid_data_france_tbl, aes(date, cases_total)) +
  geom_line(data = covid_data_spain_tbl, aes(date, cases_total)) +
  geom_line(data = covid_data_usa_tbl, aes(date, cases_total))

# 4.2 Mortality rate world map

world <- map_data("world")

plot <- mortality_rate_tbl %>%
  left_join(world, by = c("countriesAndTerritories" = "region")) %>% 
  ggplot(aes(x = long, y = lat, color = mortality_rate)) +
  geom_map(aes(fill = mortality_rate), map = world)

# I didn't manage to finish the last two plots, but I worked a whole week on
# the challenges and learned a lot which I will be able to use in the following
# two data science courses. I hope that I passed this course anyway.












