
# 1.0 LIBRARIES ----

library(tidyverse)

# 2.0 DATA IMPORT ----

covid_data_tbl <- 
  read_csv("https://opendata.ecdc.europa.eu/covid19/casedistribution/csv")

# 3.0 DATA WRANGLING

covid_data_wrangled_tbl <- covid_data_tbl %>% 
  select(dateRep, cases, deaths, countriesAndTerritories, countryterritoryCode, `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`) %>% 
  rename(cumulative_14_days_per_100000 = `Cumulative_number_for_14_days_of_COVID-19_cases_per_100000`)
  
covid_data_germany_tbl <- covid_data_wrangled_tbl %>% 
  filter(countriesAndTerritories == "Germany") %>% 
  mutate(cases_total = cases) %>% 
  select(dateRep, cases, cases_total, deaths, countriesAndTerritories, countryterritoryCode, cumulative_14_days_per_100000)

for (i in covid_data_germany_tbl$cases_total) {
  if ((i >= 2) && (i <= nrow(covid_data_germany_tbl) + 1)) {
    covid_data_germany_tbl$cases_total[i] <- 
      covid_data_germany_tbl$cases_total[i - 1] + covid_data_germany_tbl$cases[i]
  }
}
