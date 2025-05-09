# 1. Preparing data for plotting

# INTERACTIVE PLOTS ----

# GOAL: DEVELOP INTERACTIVE PLOTS FOR A SALES REPORT

# LIBRARIES & DATA ----

# Main
library(tidyverse)
library(lubridate)

# Visualization
library(plotly)

bikes_tbl      <- readRDS("../00_bike_data/bikes_tbl.rds")
bikeshops_tbl  <- readRDS("../00_bike_data/bikeshops_tbl.rds")
orderlines_tbl <- readRDS("../00_bike_data/orderlines_tbl.rds")

bike_orderlines_tbl <- orderlines_tbl %>%
  left_join(bikes_tbl,     by = c("product_id" = "bike_id")) %>%
  left_join(bikeshops_tbl, by = c("customer_id" = "bikeshop_id")) %>%
  
  # Add the total price
  mutate(total_price = price_euro * quantity)

# EURO formatting

format_to_euro <- function(x, suffix = " €") {
  
  scales::dollar(x,
                 suffix       = suffix,
                 prefix       = "",
                 big.mark     = ".",
                 decimal.mark = ",")
}

euro_format <- function(scale        = 1,
                        prefix       = "",
                        suffix       = " €",
                        big.mark     = ".",
                        decimal.mark = ",") {
  
  scales::dollar_format(suffix       = suffix,
                        prefix       = prefix,
                        big.mark     = big.mark,
                        decimal.mark = decimal.mark,
                        scale        = scale)
  
}

# 1.0 TOTAL SALES BY MONTH ----

# 1.1 Preparing Time Series Data ----
# Monthly

total_sales_m_tbl <- bike_orderlines_tbl %>%
  
  select(order_date, total_price) %>%
  
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}"))

total_sales_m_tbl

?format 
# Scroll down to Details and go to to date-times (click format.POSIXct)
# All Abbreviations are listed here as well

"2011-01-01 00:00:00" %>% as_datetime() %>% format("%B %Y")
## [1] "January 2011"


# 2. Create a static plot

# 1.2 Interactive Plot ----

# Step 1: Create ggplot with text feature

g1 <- total_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales)) +
  
  # Geoms
  geom_point(aes(text = label_text)) +
  geom_smooth(method = "loess", span = 0.2) +
  
  # Formatting
  
  # Convert scale to euro format
  scale_y_continuous(labels = euro_format()) +
  
  # Make sure 0 will always be shown (even if the data is far away)
  expand_limits(y = 0) +
  
  labs(
    title = "Total Sales",
    y = "Revenue (EUR)",
    x = ""
  )

g1


# 3. Create a dynamic plot

# Step 2: Use ggplotly()
ggplotly(g1)

ggplotly(g1, tooltip = "text")


### CUSTOM FUNCTION OF PREVIOUS STUFF ###

# 1.3 Plot Total Sales Function ----

plot_total_sales <- function(unit = "month", date_format = "%B %Y", interactive = TRUE) {
  
  # Handle Data
  data_tbl <- bike_orderlines_tbl %>%
    
    select(order_date, total_price) %>%
    
    mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
    
    group_by(date_rounded) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}"))
  
  # Make Plot
  g1 <- data_tbl %>%
    ggplot(aes(x = date_rounded, y = total_sales)) +
    
    # Geoms
    geom_point(aes(text = label_text), color = "#2C3E50") +
    geom_smooth(method = "loess", span = 0.2) +
    
    # Formatting
    scale_y_continuous(labels = euro_format()) +
    expand_limits(y = 0) +
    labs(
      title = "Total Sales",
      y = "Revenue (Euro)",
      x = ""
    )
  
  # Static vs Interactive Logic
  if (interactive) {
    return(ggplotly(g1, tooltip = "text"))
  } else {
    return(g1)
  }
  
}

# 1.4 Test Our Function ----

plot_total_sales(unit = "weekly", date_format = "%B %d, %Y", interactive = TRUE)

plot_total_sales(unit = "monthly", date_format = "%B %Y", interactive = TRUE)


##### EXERCISE #####

# 2.0 CATEGORY 2 SALES BY MONTH ----

# 2.1 Preparing Time Series Data ----

category_2_sales_m_tbl <- bike_orderlines_tbl %>%
  select(order_date, category_1, category_2, total_price) %>%
  mutate(date_rounded = floor_date(order_date, unit = "month")) %>%
  
  group_by(date_rounded, category_1, category_2) %>%
  summarise(total_sales = sum(total_price)) %>%
  ungroup() %>%
  
  mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format('%B %Y')}")) %>%
  
  mutate(category_2 = as_factor(category_2) %>%
           fct_reorder2(date_rounded, total_sales))

# 2.2 Interactive Plot ----

# Step 1: Create ggplot
g2 <- category_2_sales_m_tbl %>%
  ggplot(aes(x = date_rounded, y = total_sales, color = category_2)) +
  
  # Geoms
  geom_point(aes(text = label_text)) +
  geom_smooth(method = "loess", span = 0.2) +
  facet_wrap(~ category_2, scales = "free_y", ncol = 3) +
  
  # Formatting
  expand_limits(y = 0) +
  theme(legend.position = "none",
        # Change the height so the text looks less squished
        strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
  scale_y_continuous(labels = euro_format(scale = 1e-3, suffix = "K €")) +
  labs(
    title = "Sales By Category 2",
    y = "", x = ""
  )

# Step 2: Use ggplotly()
ggplotly(g2, tooltip = "text")

# Step 3: Make it a function, which adds flexibility of input terms
plot_categories <- function(category_1 = "All", category_2 = "All",
                            unit = "month", date_format = "%B %Y",
                            ncol = 1, scales = "free_y",
                            interactive = TRUE) { }

# Examples of running the function (Use | as an OR operator)
plot_categories(category_1 = "All", 
                category_2 = "(Gravel|Cyclo|Fat)", 
                unit = "month",
                ncol = 1, 
                scales = "free_y", 
                date_format = "%Y-%m-%d")

plot_categories(category_1 = "All", 
                category_2 = "Endurance", 
                unit = "day",
                ncol = 1, 
                scales = "free_y", 
                date_format = "%Y-%m-%d")

plot_categories(category_1 = "(Gravel|Mountain)", 
                category_2 = "All", 
                unit = "quarter",
                ncol = 2, 
                scales = "free_y", 
                date_format = "%Y-%m-%d")

# Handle Inputs
cat_1_text <- str_to_lower(category_1)
cat_2_text <- str_to_lower(category_2)

# Create Filter Logic
if (cat_1_text != "all") {
  data_tbl <- data_tbl %>%
    filter(category_1 %>%
             str_to_lower() %>%
             str_detect(pattern = cat_1_text))
}

if (cat_2_text != "all") {
  data_tbl <- data_tbl %>%
    filter(category_2 %>%
             str_to_lower() %>%
             str_detect(pattern = cat_2_text))
}

# 2.3 Plot Categories Function ----

plot_categories <- function(category_1 = "All", category_2 = "All",
                            unit = "month", date_format = "%B %Y",
                            ncol = 1, scales = "free_y",
                            interactive = TRUE) {
  
  # Handle Data
  
  data_tbl <- bike_orderlines_tbl %>%
    select(order_date, category_1, category_2, total_price) %>%
    mutate(date_rounded = floor_date(order_date, unit = unit)) %>%
    
    group_by(date_rounded, category_1, category_2) %>%
    summarise(total_sales = sum(total_price)) %>%
    ungroup() %>%
    
    mutate(label_text = str_glue("Sales: {format_to_euro(total_sales)}
                                 Date: {date_rounded %>% format(date_format)}")) %>%
    
    mutate(category_2 = as_factor(category_2) %>%
             fct_reorder2(date_rounded, total_sales))
  
  # Handle Inputs
  cat_1_text <- str_to_lower(category_1)
  cat_2_text <- str_to_lower(category_2)
  
  # Create Filter Logic
  if (cat_1_text != "all") {
    data_tbl <- data_tbl %>%
      filter(category_1 %>%
               str_to_lower() %>%
               str_detect(pattern = cat_1_text))
  }
  
  if (cat_2_text != "all") {
    data_tbl <- data_tbl %>%
      filter(category_2 %>%
               str_to_lower() %>%
               str_detect(pattern = cat_2_text))
  }
  
  # Make Plot
  g2 <- data_tbl %>%
    ggplot(aes(x = date_rounded, y = total_sales, color = category_2)) +
    
    # Geoms
    geom_point(aes(text = label_text), color = "#2c3e50") +
    geom_smooth(method = "loess", span = 0.2) +
    facet_wrap(~ category_2, scales = scales, ncol = ncol) +
    
    # Formatting
    expand_limits(y = 0) +
    theme(legend.position = "none",
          strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
    scale_y_continuous(labels = euro_format(scale = 1e-3, suffix = "K €")) +
    labs(
      title = "Sales By Category 2",
      y = "", x = ""
    )
  
  # Static Vs Interactive Logic
  if (interactive) {
    return(ggplotly(g2, tooltip = "text"))
  } else {
    return(g2)
  }
  
}

# 3.0 SAVE FUNCTIONS ----
# 3.1 Create a file
fs::file_create("../00_scripts/plot_sales.R")

# 3.2 Save functions to the file
dump(list = c("plot_total_sales", 
              "plot_categories", 
              "format_to_euro", 
              "euro_format"), 
     file = "../00_scripts/plot_sales.R")
