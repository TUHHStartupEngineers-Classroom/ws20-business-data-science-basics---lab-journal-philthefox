
# 1.0 LIBRARIES ----

library(tidyverse)
library(lubridate)

# 2.0 DATA IMPORT ----

covid_data_tbl <- 
  read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# 3.0 DATA WRANGLING

covid_data_wrangled_tbl <- covid_data_tbl %>% 
  mutate(date = dmy(dateRep)) %>% 
  select(date, cases, deaths, countriesAndTerritories, countryterritoryCode, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`) %>% 
  rename(cumulative_14_days_per_100000 = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`) %>% 
  arrange(date)
  
covid_data_germany_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "Germany") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

covid_data_france_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "France") %>% 
  mutate(cases_total = cumsum(cases)) %>% 
  select(date, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

test_tbl <- bind_rows(covid_data_germany_tbl, covid_data_france_tbl)

covid_data_germany_tbl %>% 
  ggplot(aes(date, cases_total)) +
  geom_line(size = 1) +
  geom_line(data = covid_data_france_tbl, aes(date, cases_total))


