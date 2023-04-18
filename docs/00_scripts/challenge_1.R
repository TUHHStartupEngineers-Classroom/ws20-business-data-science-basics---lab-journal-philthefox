# 1.0 Load libraries ----
library(tidyverse)
library(readxl)
library(lubridate)

# 2.0 Importing Files ----
bikes_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikes.xlsx")
orderlines_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/orderlines.xlsx")
bikeshops_tbl <- read_excel("00_data/01_bike_sales/01_raw_data/bikeshops.xlsx")

# 3.0 Examining Data ----


# 4.0 Joining Data ----
bike_orderlines_joined_tbl <- orderlines_tbl %>% 
  left_join(bikes_tbl, by = c("product.id" = "bike.id")) %>% 
  left_join(bikeshops_tbl, by = c("customer.id" = "bikeshop.id"))

# 5.0 Wrangling Data ----
bike_orderlines_wrangled_tbl <- bike_orderlines_joined_tbl %>% 
  separate(col = "location",
           into = c("city", "state"),
           sep = ", ") %>% 
  mutate(total_price = quantity * price) %>% 
  select(-...1, -gender, -ends_with(".id")) %>% 
  bind_cols(bike_orderlines_joined_tbl %>% select(order.id)) %>% 
  select(order.id,
         contains("order"),
         contains("model"),
         contains("category"),
         price,
         quantity,
         total_price,
         everything()) %>% 
  rename("bikeshop" = "name") %>% 
  set_names(names(.) %>% str_replace_all("\\.", "_"))

# 6.0 Business Insights ----
# 6.1 Analyze the sales by location (state) with a bar plot. ----
#     Since state and city are multiple features (variables), they should be
#     split. Which state has the highest revenue?

# Step 1 - Manipulate
sales_by_state_tbl <- bike_orderlines_wrangled_tbl %>% 
  select(total_price, state) %>% 
  group_by(state) %>% 
  summarize(sales = sum(total_price)) %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_state_tbl %>% 
  ggplot(aes(x = state, y = sales)) +
  geom_col(fill = "#2DC6D6") +
  geom_label(aes(label = sales_text)) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Revenue by state",
    x = "",
    y = "Revenue"
  )

# 6.2 Analyze the sales by location and year (facet_wrap). ----

# Step 1 - Manipulate
sales_by_year_state_tbl <- bike_orderlines_wrangled_tbl %>% 
  transmute(year = year(order_date), total_price, state) %>% 
  group_by(year, state) %>% 
  summarize(sales = sum(total_price)) %>% 
  ungroup() %>% 
  mutate(sales_text = scales::dollar(sales, big.mark = ".",
                                     decimal.mark = ",",
                                     prefix = "",
                                     suffix = " €"))

# Step 2 - Visualize
sales_by_year_state_tbl %>% 
  ggplot(aes(x = year, y = sales, fill = state)) +
  geom_col() +
  facet_wrap(~ state) +
  scale_y_continuous(labels = scales::dollar_format(big.mark = ".",
                                                    decimal.mark = ",",
                                                    prefix = "",
                                                    suffix = " €")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(
    title = "Revenue by year and state",
    fill = "Main category"
  )