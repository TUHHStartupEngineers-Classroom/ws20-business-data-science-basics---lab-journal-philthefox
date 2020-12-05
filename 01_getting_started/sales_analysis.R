# Data Science at TUHH ------------------------------------------------------
# SALES ANALYSIS ----

# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
# bikeshops_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----


# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) ## %>% 
  # left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = "category",
         into = c("category.1", "category.2", "category.3"),
         sep = " - ") %>% 
  mutate(total.price = quantity * price) %>% 
  select(-...1, -gender, -ends_with(".id")) %>% 
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id, contains("order"), contains("model"), contains("category"), price, quantity, total.price, everything()) %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))
  
# 6.0 Business Insights ----
# 6.1 Sales by Year ----

# Step 1 - Manipulate
sales_by_year_tbl <- bike_orderlines_wrangled_tbl %>% 
  transmute(year = year(order_date), total_price) %>% 
  group_by(year) %>% 
  summarize(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_tbl %>% 
  ggplot(aes(x = year, y = sales)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year",
    subtitle = "Upward trend",
    x = "",
    y = "Revenue"
  )

# 6.2 Sales by Year and Category 2 ----

# Step 1 - Manipulate
sales_by_year_category_1_tbl <- bike_orderlines_wrangled_tbl %>% 
  transmute(year = year(order_date), category_1, total_price) %>% 
  group_by(year, category_1) %>% 
  summarize(sales = sum(total_price)) %>% 
  ungroup() %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))


# Step 2 - Visualize
sales_by_year_category_1_tbl %>% 
  ggplot(aes(x = year, y = sales, fill = category_1)) +
  geom_col() +
  facet_wrap(~ category_1) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  labs(
    title = "Revenue by year and main category",
    subtitle = "Each product category has an upward trend",
    fill = "Main category"
  )


# 7.0 Writing Files ----

# 7.1 Excel ----

# 7.2 CSV ----

# 7.3 RDS ----

