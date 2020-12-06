library("tidyverse")

# Make data tidy

# pivot_longer(): If some of the column names are actually values of a variable

diamonds2 <- readRDS("00_data/diamonds2.rds")

diamonds2_tidy <- diamonds2 %>%
  pivot_longer(cols = c("2008", "2009"),
               names_to = "year",
               values_to = "price")

# pivot_wider(): If some of the observations are scattered across many rows

diamonds3 <- readRDS("00_data/diamonds3.rds")

diamonds3_tidy <- diamonds3 %>% 
  pivot_wider(names_from = "dimension",
              values_from = "measurement")

# separate(): If some of the columns contain more than one value

diamonds4 <- readRDS("00_data/diamonds4.rds")

diamonds4_tidy <- diamonds4 %>% 
  separate(col = "dim",
           into = c("x", "y", "z"),
           sep = "/",
           convert = TRUE)

# unite(): use to paste together multiple columns into one






