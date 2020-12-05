
# Get some data via an API ----

# 1.0 LIBRARIES ----

library(httr)
library(glue)
library(jsonlite)

# 2.0 GET DATA ----
# Get information about countries
response <- GET("https://restcountries.eu/rest/v2/all")

countries <- response$content %>%
  rawToChar() %>%
  fromJSON() %>% 
  as_tibble()

head(countries, 10)



# Scrape one of the competitor websites of canyon and create a small database.
# I chose Rose Bikes

# 1.0 LIBRARIES ----

library(tidyverse) # Main Package - Loads dplyr, purrr, etc.
library(rvest)     # HTML Hacking & Web Scraping
library(xopen)     # Quickly opening URLs
library(jsonlite)  # converts JSON files to R objects
library(glue)      # concatenate strings
library(stringi)   # character string/text processing
# library(dplyr)

# 1.1 COLLECT PRODUCT FAMILIES ----

url_home          <- "https://www.rosebikes.de/fahrräder"
# xopen(url_home) # Open links directly from RStudio to inspect them

# Read in the HTML for the entire webpage
html_home <- read_html(url_home)

# Web scrape the ids for the families
bike_family_name_tbl <- html_home %>%
  
# Get the nodes for the families ...
html_nodes(css = ".catalog-navigation__link") %>%
# ... and extract the information of the id attribute
html_text() %>%

stringr::str_replace_all("\\n", "") %>% 

# Remove the product families Gear and Outlet and Woman 
# (because the female bikes are also listed with the others)
discard(.p = ~stringr::str_detect(.x,"Sale|Bike-Finder")) %>%

# Convert vector to tibble
enframe(name = "product_family_id", value = "product_family")

# 1.2 COLLECT PRODUCT CATEGORIES ----

# Web scrape the ids for the urls of the families
bike_family_url_tbl <- html_home %>%
  
# Get the nodes for the families ...
html_nodes(css = ".catalog-navigation__link") %>%
# ... and extract the information of the id attribute
html_attr('href') %>%

# Remove the product families Gear and Outlet and Woman 
# (because the female bikes are also listed with the others)
discard(.p = ~stringr::str_detect(.x,"/fahrräder/sale|/zoovu")) %>%

# Convert vector to tibble
enframe(name = "product_family_id", value = "subdirectory") %>%

# Add the domain, because we will get only the subdirectories
mutate(
  url = glue("https://www.rosebikes.de{subdirectory}")
)

# Combine tibbles with names and URLs of the product families
bike_family_tbl <- bike_family_name_tbl %>% 
  left_join(bike_family_url_tbl)



# 3.0

get_bike_category_tbl <- function(subdirectory_family) {
  url_family <- glue("https://www.rosebikes.de{subdirectory_family}")
  # xopen(url_family) # Open links directly from RStudio to inspect them
  
  # Read in the HTML for the product family page
  html_family <- read_html(url_family)
  
  # Names
  # Web scrape the ids for the families
  bike_category_name_tbl <- html_family %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-navigation__link") %>%
    # ... and extract the information of the id attribute
    html_text() %>%
    
    stringr::str_replace_all("\\n", "") %>% 
    
    # Remove the product families Gear and Outlet and Woman 
    # (because the female bikes are also listed with the others)
    discard(.p = ~stringr::str_detect(.x,"Alle")) %>%
    
    # Convert vector to tibble
    enframe(name = "position", value = "product_family")
  
  # URLs
  # Web scrape the ids for the URLs of the categories
  bike_category_url_tbl <- html_family %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-navigation__link") %>%
    # ... and extract the information of the id attribute
    html_attr('href') %>%
    
    # Remove the first URL since it's the same as the subdirectory
    discard(.p = ~stringr::str_ends(.x, subdirectory_family)) %>%
    
    # Convert vector to tibble
    enframe(name = "position", value = "subdirectory") %>%
    
    # Add the domain, because we will get only the subdirectories
    mutate(
      url = glue("https://www.rosebikes.de{subdirectory}")
    )
  
  # Combine tibbles with names and URLs of the product categories
  bike_category_tbl <- bike_category_name_tbl %>% 
    left_join(bike_category_url_tbl) %>% 
    # Delete positions
    select(-position)
}

test <- map(bike_family_tbl$subdirectory, get_bike_category_tbl)

# 4.0

get_bike_data <- function(subdirectory_id) {
  
  subdirectory_family <- bike_family_tbl$subdirectory[subdirectory_id]
  url_family <- glue("https://www.rosebikes.de{subdirectory_family}")
  # xopen(url_family) # Open links directly from RStudio to inspect them
  
  # Read in the HTML for the product family page
  html_family <- read_html(url_family)
  
  # Names
  # Web scrape the ids for the families
  bike_name_tbl <- html_family %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-bikes__title-text") %>%
    # ... and extract the information of the id attribute
    html_text() %>%
    
    stringr::str_replace_all("\\n", "") %>% 
    
    # Convert vector to tibble
    enframe(name = "position", value = "name")
  
  # Descriptions
  # Web scrape the ids for the families
  bike_description_tbl <- html_family %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-bikes__subtitle") %>%
    # ... and extract the information of the id attribute
    html_text() %>%
    
    stringr::str_replace_all("\\n", "") %>% 
    
    # Convert vector to tibble
    enframe(name = "position", value = "description")
  
  # Prices
  # Web scrape the ids for the families
  bike_price_tbl <- html_family %>%
    
    # Get the nodes for the families ...
    html_nodes(css = ".catalog-category-bikes__price-title") %>%
    # ... and extract the information of the id attribute
    html_text() %>%
    
    # Extract numbers
    stringr::str_extract(pattern = "[0-9].[0-9]*,[0-9]*") %>%
    stringr::str_replace_all(pattern = "[.]", replacement = "") %>%
    stringr::str_replace_all(pattern = ",", replacement = ".") %>%
    as.numeric() %>% 
    
    # Convert vector to tibble
    enframe(name = "position", value = "price_from")
  
  # Create tibble with id of subdirectory
  product_family_id <- NULL
  for (i in c(1:nrow(bike_price_tbl))) {
    product_family_id[i] = subdirectory_id;
  }
  product_family_id_tbl <- enframe(product_family_id, name = "position", value = "product_family_id")
  
  # Combine tibbles with names, descriptions and prices of the product categories
  bike_tbl <- bike_name_tbl %>% 
    left_join(bike_description_tbl) %>% 
    left_join(bike_price_tbl) %>% 
    left_join(product_family_id_tbl) %>% 
    # Delete positions
    select(-position)
}

bike_data_lst <- map(1:nrow(bike_family_tbl), get_bike_data)
bike_data_tbl <- bind_rows(bike_data_lst)

bike_data_tbl <- bike_data_tbl %>% 
  left_join(bike_family_tbl) %>% 
  # Delete product_family_id, subdirectory and url
  select(-product_family_id, -subdirectory, -url)

head(bike_data_tbl, 10)









