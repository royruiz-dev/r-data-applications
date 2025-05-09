plot_total_sales <-
  function(data, unit = "month", date_format = "%B %Y", shiny = FALSE, interactive = TRUE) {
    
    # Handle Data
    data_tbl <- data %>%
      dplyr::select(order_date, total_price) %>%
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
      scale_x_date(breaks = "16 weeks", date_labels = "%b %d") +
      # Uses minimal theme for minimal look
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            axis.text = element_text(size = "10"),
            legend.position = "none",
            strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
      labs(y = "Adjusted Share Price",
           x = "")
    labs(
      title = "Total Sales",
      y = "Revenue (Euro)",
      x = ""
    )
    
    if (shiny){
      # Margins for Time Plots in Shiny App
      margin_size <- list(l = 20, r = 20, b = 160, t = 60)
      # Static vs Interactive Logic
      if (interactive) {
        return(ggplotly(g1, tooltip = "text") %>% layout(autosize = T, margin = margin_size))
      } else {
        return(g1 %>% layout(autosize = T, margin = margin_size))
      }
    }
    else {
      # Static vs Interactive Logic
      if (interactive) {
        return(ggplotly(g1, tooltip = "text"))
      } else {
        return(g1)
      }
    }
  }

plot_categories <-
  function(data, category_1 = "All", category_2 = "All",
           unit = "month", date_format = "%B %Y",
           ncol = 1, scales = "free_y",
           interactive = TRUE) {
    
    # Handle Data
    
    data_tbl <- data %>%
      dplyr::select(order_date, category_1, category_2, total_price) %>%
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
      scale_y_continuous(labels = euro_format(scale = 1e-3, suffix = "K €")) +
      scale_x_date(breaks = "16 weeks", date_labels = "%b %d") +
      # Uses minimal theme for minimal look
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            plot.title = element_text(face = "bold"),
            axis.text = element_text(size = "10"),
            legend.position = "none",
            strip.text.x = element_text(margin = margin(5, 5, 5, 5, unit = "pt"))) +
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
format_to_euro <-
  function(x, suffix = " €") {
    
    scales::dollar(x,
                   suffix       = suffix,
                   prefix       = "",
                   big.mark     = ".",
                   decimal.mark = ",")
  }

format_to_million_dollar <-
  function(x, scale = 1e-6, suffix = "M", prefix = "$") {
    
    scales::dollar(x,
                   suffix       = suffix,
                   prefix       = prefix,
                   big.mark     = ",",
                   decimal.mark = ".",
                   scale        = scale)
  }

euro_format <-
  function(scale        = 1,
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
