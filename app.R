options(stringsAsFactors = FALSE)

# ==========================================================
# Carolina Hurricanes F&B Analytics Shiny App
# Full client-ready app for RQ1, RQ2, and RQ3
# Keeps the same simple Shiny theme style (fluidPage + default look)
# Author: Saurabh Gupta
# Date: Apr 26
# ==========================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readr)
  library(lubridate)
  library(tidyr)
  library(ggplot2)
  library(DT)
  library(broom)
  library(scales)
})

# -----------------------------
# helper functions
# -----------------------------
get_season_year <- function(x) {
  case_when(
    x >= as.Date("2023-10-01") & x < as.Date("2024-08-01") ~ "2023-24",
    x >= as.Date("2024-08-01") & x < as.Date("2025-08-01") ~ "2024-25",
    x >= as.Date("2025-08-01")                               ~ "2025-26",
    TRUE                                                     ~ NA_character_
  )
}

rmse <- function(actual, predicted) {
  sqrt(mean((actual - predicted)^2, na.rm = TRUE))
}

mape <- function(actual, predicted) {
  mean(abs((actual - predicted) / actual), na.rm = TRUE) * 100
}

safe_log <- function(x) {
  ifelse(is.na(x) | x <= 0, NA_real_, log(x))
}

predict_backtransform <- function(model, newdata) {
  pred <- predict(model, newdata = newdata, interval = "confidence")
  out <- as_tibble(pred) %>%
    mutate(
      fit_response = exp(fit),
      lwr_response = exp(lwr),
      upr_response = exp(upr)
    )
  bind_cols(newdata, out)
}

elasticity_label <- function(beta) {
  dplyr::case_when(
    is.na(beta) ~ "Not available",
    beta < -1 ~ "Elastic",
    beta <= 0 & beta >= -1 ~ "Inelastic",
    beta > 0 ~ "Positive estimate; interpret with caution",
    TRUE ~ "Review estimate"
  )
}

revenue_change_pct <- function(price_change_pct, elasticity) {
  delta <- price_change_pct / 100
  100 * (((1 + delta) * (1 + elasticity * delta)) - 1)
}

# -----------------------------
# read data
# -----------------------------
data_dir <- "data"
lineitems_file <- file.path(data_dir, "lineitems_data_v2.csv")
orders_file    <- file.path(data_dir, "orders_data_v2.csv")
events_file    <- file.path(data_dir, "event_data.csv")
ticket_file    <- file.path(data_dir, "ticket_data_v2.csv")

required_files <- c(lineitems_file, orders_file, events_file, ticket_file)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    paste0(
      "Missing required file(s): ",
      paste(missing_files, collapse = ", "),
      ". Put the CSV files inside the data folder before running the app."
    )
  )
}

lineitems_raw <- read_csv(lineitems_file, show_col_types = FALSE)
orders_raw    <- read_csv(orders_file, show_col_types = FALSE)
events_raw    <- read_csv(events_file, show_col_types = FALSE)
ticket_raw    <- read_csv(ticket_file, show_col_types = FALSE)

# -----------------------------
# clean data
# -----------------------------
orders_clean <- orders_raw %>%
  mutate(
    order_id = as.integer(order_id),
    event_date = as.Date(event_date)
  )

events_clean <- events_raw %>%
  mutate(
    event_date = parse_date_time(event_date, orders = c("mdy", "ymd", "dmy")) |> as.Date(),
    total_attendance = as.numeric(total_attendance),
    season_year = get_season_year(event_date),
    is_weekend = if_else(wday(event_date, week_start = 1) %in% c(5, 6), 1L, 0L),
    is_playoff = if_else(
      (event_date >= as.Date("2024-04-20") & event_date <= as.Date("2024-06-24")) |
        (event_date >= as.Date("2025-04-19") & event_date <= as.Date("2025-06-17")),
      1L, 0L
    )
  ) %>%
  filter(!is.na(event_date), !is.na(total_attendance), !is.na(season_year))

if ("event_date" %in% names(ticket_raw)) {
  ticket_clean <- ticket_raw %>%
    mutate(
      event_date = as.Date(event_date),
      block_price = as.numeric(block_price)
    )
} else if ("eventdatetime" %in% names(ticket_raw)) {
  ticket_clean <- ticket_raw %>%
    mutate(
      event_date = as.Date(mdy_hm(eventdatetime)),
      block_price = as.numeric(block_price)
    )
} else {
  stop("ticket_data_v2.csv must contain either event_date or eventdatetime")
}

ticket_clean <- ticket_clean %>%
  filter(!is.na(event_date), !is.na(block_price), block_price > 0)

lineitems_clean <- lineitems_raw %>%
  filter(refunded == 0, voided == 0, quantity > 0, price > 0) %>%
  mutate(
    order_id = as.integer(order_id),
    category = case_when(
      category %in% c("Pkg Beer", "Draft Beer") ~ "Beer",
      category == "NA Bev" ~ "Non-Alc Bev",
      TRUE ~ category
    )
  ) %>%
  filter(!category %in% c("Retail", "Service Charge")) %>%
  left_join(orders_clean %>% select(order_id, event_date), by = "order_id") %>%
  filter(!is.na(event_date))

# -----------------------------
# event master
# -----------------------------
ticket_game <- ticket_clean %>%
  group_by(event_date) %>%
  summarise(
    avg_ticket_price = mean(block_price, na.rm = TRUE),
    median_ticket_price = median(block_price, na.rm = TRUE),
    sd_ticket_price = sd(block_price, na.rm = TRUE),
    n_tickets = n(),
    .groups = "drop"
  )

orders_game <- orders_clean %>%
  filter(!is.na(subtotal), subtotal > 0) %>%
  group_by(event_date) %>%
  summarise(
    total_orders = n(),
    avg_order_value = mean(subtotal, na.rm = TRUE),
    total_order_subtotal = sum(subtotal, na.rm = TRUE),
    .groups = "drop"
  )

event_master <- events_clean %>%
  left_join(ticket_game, by = "event_date") %>%
  left_join(orders_game, by = "event_date") %>%
  arrange(event_date) %>%
  group_by(season_year) %>%
  mutate(
    season_game_number = row_number(),
    season_tercile_num = ntile(season_game_number, 3),
    season_tercile = case_when(
      season_tercile_num == 1 ~ "Early",
      season_tercile_num == 2 ~ "Middle",
      season_tercile_num == 3 ~ "Late"
    )
  ) %>%
  ungroup() %>%
  mutate(
    season_year = factor(season_year, levels = c("2023-24", "2024-25", "2025-26")),
    season_tercile = factor(season_tercile, levels = c("Early", "Middle", "Late")),
    is_weekend = factor(is_weekend, levels = c(0, 1)),
    is_playoff = factor(is_playoff, levels = c(0, 1))
  )

# -----------------------------
# RQ1 category-game data
# -----------------------------
category_game <- lineitems_clean %>%
  group_by(event_date, category) %>%
  summarise(
    total_qty = sum(quantity, na.rm = TRUE),
    avg_price = mean(price, na.rm = TRUE),
    median_price = median(price, na.rm = TRUE),
    total_revenue = sum(quantity * price, na.rm = TRUE),
    n_lineitems = n(),
    .groups = "drop"
  ) %>%
  left_join(
    event_master %>%
      select(event_date, total_attendance, avg_ticket_price, median_ticket_price,
             is_playoff, is_weekend, season_year, season_tercile),
    by = "event_date"
  ) %>%
  mutate(
    qty_per_capita = total_qty / total_attendance,
    rev_per_capita = total_revenue / total_attendance,
    log_qty_per_capita = safe_log(qty_per_capita),
    log_avg_price = safe_log(avg_price),
    log_attendance = safe_log(total_attendance),
    log_avg_ticket_price = safe_log(avg_ticket_price)
  ) %>%
  filter(
    !is.na(log_qty_per_capita),
    !is.na(log_avg_price),
    !is.na(log_attendance),
    !is.na(log_avg_ticket_price)
  )

# -----------------------------
# RQ2 item-game data
# -----------------------------
item_game <- lineitems_clean %>%
  group_by(event_date, item_name, category) %>%
  summarise(
    total_qty = sum(quantity, na.rm = TRUE),
    total_revenue = sum(quantity * price, na.rm = TRUE),
    avg_price = total_revenue / total_qty,
    .groups = "drop"
  ) %>%
  left_join(
    event_master %>%
      select(event_date, total_attendance, avg_ticket_price,
             is_playoff, is_weekend, season_year, season_tercile),
    by = "event_date"
  ) %>%
  mutate(
    qty_per_capita = total_qty / total_attendance,
    rev_per_capita = total_revenue / total_attendance,
    log_qty_per_capita = safe_log(qty_per_capita),
    log_avg_price = safe_log(avg_price),
    log_attendance = safe_log(total_attendance),
    log_avg_ticket_price = safe_log(avg_ticket_price)
  ) %>%
  filter(
    !is.na(log_qty_per_capita),
    !is.na(log_avg_price),
    !is.na(log_attendance),
    !is.na(log_avg_ticket_price)
  )

valid_items <- item_game %>%
  group_by(item_name, category) %>%
  summarise(
    n_games = n(),
    price_sd = sd(avg_price, na.rm = TRUE),
    n_seasons = n_distinct(season_year),
    avg_qty = mean(total_qty, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  filter(n_games >= 10, !is.na(price_sd), price_sd > 0, n_seasons >= 2) %>%
  arrange(desc(avg_qty), item_name)

# -----------------------------
# RQ3 event-level data
# -----------------------------
event_revenue <- lineitems_clean %>%
  group_by(event_date) %>%
  summarise(
    event_fnb_revenue = sum(quantity * price, na.rm = TRUE),
    event_fnb_qty = sum(quantity, na.rm = TRUE),
    .groups = "drop"
  )

rq3_event <- event_master %>%
  left_join(event_revenue, by = "event_date") %>%
  mutate(
    fnb_rev_per_cap = event_fnb_revenue / total_attendance,
    fnb_qty_per_cap = event_fnb_qty / total_attendance,
    log_fnb_rev_per_cap = safe_log(fnb_rev_per_cap),
    log_avg_ticket_price = safe_log(avg_ticket_price),
    log_attendance = safe_log(total_attendance)
  ) %>%
  filter(
    !is.na(log_fnb_rev_per_cap),
    !is.na(log_avg_ticket_price),
    !is.na(log_attendance)
  )

# -----------------------------
# global choices
# -----------------------------
all_categories <- sort(unique(category_game$category))
all_seasons <- levels(event_master$season_year)
all_terciles <- levels(event_master$season_tercile)
all_items <- valid_items$item_name

storm_default <- all_items[grepl("Storm Brew", all_items, ignore.case = TRUE)][1]
if (is.na(storm_default)) storm_default <- all_items[1]

# -----------------------------
# ui
# -----------------------------
ui <- fluidPage(
  titlePanel("Carolina Hurricanes F&B Price Elasticity App"),
  sidebarLayout(
    sidebarPanel(
      h4("Global Filters"),
      checkboxGroupInput(
        "season_filter", "Seasons:",
        choices = all_seasons,
        selected = all_seasons
      ),
      checkboxInput("include_playoffs", "Include playoff games", value = TRUE),
      sliderInput("attendance_range", "Attendance range:",
                  min = floor(min(event_master$total_attendance, na.rm = TRUE)),
                  max = ceiling(max(event_master$total_attendance, na.rm = TRUE)),
                  value = c(floor(min(event_master$total_attendance, na.rm = TRUE)),
                            ceiling(max(event_master$total_attendance, na.rm = TRUE)))),
      hr(),

      h4("Global Scenario Controls"),
      sliderInput("global_attendance_scn", "Scenario attendance",
                  min = floor(min(event_master$total_attendance, na.rm = TRUE)),
                  max = ceiling(max(event_master$total_attendance, na.rm = TRUE)),
                  value = round(mean(event_master$total_attendance, na.rm = TRUE))),
      checkboxInput("global_weekend_scn", "Scenario is weekend", value = FALSE),
      checkboxInput("global_playoff_scn", "Scenario is playoff", value = FALSE),
      selectInput("global_season_scn", "Scenario season year", choices = all_seasons, selected = tail(all_seasons, 1)),
      hr(),

      h4("RQ1 Controls"),
      selectInput("rq1_category", "Category:", choices = all_categories, selected = all_categories[1]),
      sliderInput("rq1_price_change", "Category price change scenario (%)",
                  min = -20, max = 25, value = 10, step = 1),
      hr(),

      h4("RQ2 Controls"),
      selectInput("rq2_item", "Item:", choices = all_items, selected = storm_default),
      sliderInput("rq2_price_change", "Item price change scenario (%)",
                  min = -20, max = 30, value = 10, step = 1),
      hr(),

      h4("RQ3 Controls"),
      sliderInput("rq3_ticket_price", "Scenario average ticket price ($)",
                  min = floor(min(rq3_event$avg_ticket_price, na.rm = TRUE)),
                  max = ceiling(max(rq3_event$avg_ticket_price, na.rm = TRUE)),
                  value = round(mean(rq3_event$avg_ticket_price, na.rm = TRUE))),
      sliderInput("rq3_ticket_change", "Ticket price change scenario (%)",
                  min = -20, max = 30, value = 10, step = 1)
    ),

    mainPanel(
      tabsetPanel(
        tabPanel(
          "Overview",
          br(),
          h3("Project overview"),
          p("This app is designed to support the Carolina Hurricanes food and beverage pricing study."),
          p("RQ1 estimates category-level price elasticity."),
          p("RQ2 estimates item-level price elasticity for items with enough price variation and enough game observations."),
          p("RQ3 explains which game-level features are associated with F&B spending per attendee."),
          hr(),
          h4("Filtered sample summary"),
          tableOutput("overview_table"),
          h4("Key business summary"),
          verbatimTextOutput("overview_text")
        ),

        tabPanel(
          "EDA",
          br(),
          h4("Exploratory analysis"),
          plotOutput("eda_rev_ticket_plot", height = 320),
          plotOutput("eda_price_box_plot", height = 320),
          plotOutput("eda_category_rev_plot", height = 320),
          plotOutput("eda_time_plot", height = 320)
        ),

        tabPanel(
          "RQ1 Category Elasticity",
          br(),
          h4("Category model summary"),
          tableOutput("rq1_stats"),
          h4("Category scenario prediction"),
          tableOutput("rq1_prediction_table"),
          h4("Model interpretation"),
          verbatimTextOutput("rq1_text"),
          h4("Full model output"),
          verbatimTextOutput("rq1_model_output"),
          hr(),
          plotOutput("rq1_scatter", height = 320),
          plotOutput("rq1_actual_fitted", height = 320),
          plotOutput("rq1_resid_plot", height = 320),
          plotOutput("rq1_qq_plot", height = 320)
        ),

        tabPanel(
          "RQ2 Item Elasticity",
          br(),
          h4("Item model summary"),
          tableOutput("rq2_stats"),
          h4("Item scenario prediction"),
          tableOutput("rq2_prediction_table"),
          h4("Model interpretation"),
          verbatimTextOutput("rq2_text"),
          h4("Full model output"),
          verbatimTextOutput("rq2_model_output"),
          hr(),
          h4("Item price vs quantity per capita"),
          plotOutput("rq2_scatter", height = 320),
          h4("Item price over time"),
          plotOutput("rq2_price_time", height = 320),
          h4("Actual vs fitted quantity per capita"),
          plotOutput("rq2_actual_fitted", height = 320),
          h4("Model diagnostics"),
          plotOutput("rq2_resid_plot", height = 320),
          plotOutput("rq2_qq_plot", height = 320)
        ),

        tabPanel(
          "RQ3 Event Drivers",
          br(),
          h4("Event-level model summary"),
          tableOutput("rq3_stats"),
          h4("Event scenario prediction"),
          tableOutput("rq3_prediction_table"),
          h4("Model interpretation"),
          verbatimTextOutput("rq3_text"),
          h4("Full model output"),
          verbatimTextOutput("rq3_model_output"),
          hr(),
          plotOutput("rq3_scatter", height = 320),
          plotOutput("rq3_coef_plot", height = 320),
          plotOutput("rq3_actual_fitted", height = 320),
          plotOutput("rq3_resid_plot", height = 320),
          plotOutput("rq3_qq_plot", height = 320)
        ),

        tabPanel(
          "Compare Categories",
          br(),
          plotOutput("compare_plot", height = 420),
          tableOutput("compare_table")
        ),

        tabPanel(
          "Data",
          br(),
          radioButtons(
            "data_view", "Choose table:",
            choices = c(
              "Category x Game" = "cat",
              "Item x Game" = "item",
              "Event Level" = "event"
            ),
            selected = "cat",
            inline = TRUE
          ),
          downloadButton("download_data", "Download CSV"),
          br(), br(),
          DTOutput("data_table")
        )
      )
    )
  )
)

# -----------------------------
# server
# -----------------------------
server <- function(input, output, session) {

  current_data <- reactive({
    cg <- category_game %>%
      filter(
        as.character(season_year) %in% input$season_filter,
        total_attendance >= input$attendance_range[1],
        total_attendance <= input$attendance_range[2]
      )

    ig <- item_game %>%
      filter(
        as.character(season_year) %in% input$season_filter,
        total_attendance >= input$attendance_range[1],
        total_attendance <= input$attendance_range[2]
      )

    ev <- rq3_event %>%
      filter(
        as.character(season_year) %in% input$season_filter,
        total_attendance >= input$attendance_range[1],
        total_attendance <= input$attendance_range[2]
      )

    if (!isTRUE(input$include_playoffs)) {
      cg <- cg %>% filter(as.character(is_playoff) == "0")
      ig <- ig %>% filter(as.character(is_playoff) == "0")
      ev <- ev %>% filter(as.character(is_playoff) == "0")
    }

    list(category_game = cg, item_game = ig, rq3_event = ev)
  })

  rq1_data <- reactive({
    current_data()$category_game %>% filter(category == input$rq1_category)
  })

  rq2_data <- reactive({
    current_data()$item_game %>% filter(item_name == input$rq2_item)
  })

  build_formula <- function(outcome, data, candidates) {
    valid_terms <- sapply(candidates, function(term) {
      col <- data[[term]]
      if (is.null(col)) return(FALSE)
      if (is.factor(col) || is.character(col)) {
        return(length(unique(na.omit(as.character(col)))) >= 2)
      }
      return(TRUE)
    })
    kept <- candidates[valid_terms]
    as.formula(paste(outcome, "~", paste(kept, collapse = " + ")))
  }

  rq1_model <- reactive({
    req(nrow(rq1_data()) >= 8)
    f <- build_formula(
      "log_qty_per_capita",
      rq1_data(),
      c("log_avg_price", "log_attendance", "log_avg_ticket_price",
        "is_playoff", "is_weekend", "season_year", "season_tercile")
    )
    lm(f, data = rq1_data())
  })

  rq2_model <- reactive({
    req(nrow(rq2_data()) >= 10)
    f <- build_formula(
      "log_qty_per_capita",
      rq2_data(),
      c("log_avg_price", "log_attendance", "log_avg_ticket_price",
        "is_playoff", "is_weekend", "season_year")
    )
    lm(f, data = rq2_data())
  })

  rq3_model <- reactive({
    req(nrow(current_data()$rq3_event) >= 10)
    f <- build_formula(
      "log_fnb_rev_per_cap",
      current_data()$rq3_event,
      c("log_avg_ticket_price", "log_attendance",
        "is_weekend", "is_playoff", "season_tercile", "season_year")
    )
    lm(f, data = current_data()$rq3_event)
  })

  # Overview
  output$overview_table <- renderTable({
    data.frame(
      Metric = c(
        "Filtered games",
        "Filtered category-game rows",
        "Filtered item-game rows",
        "Categories in app",
        "Eligible items in app"
      ),
      Value = c(
        nrow(current_data()$rq3_event),
        nrow(current_data()$category_game),
        nrow(current_data()$item_game),
        length(unique(current_data()$category_game$category)),
        length(unique(current_data()$item_game$item_name))
      )
    )
  })

  output$overview_text <- renderText({
    avg_rev <- mean(current_data()$rq3_event$fnb_rev_per_cap, na.rm = TRUE)
    avg_ticket <- mean(current_data()$rq3_event$avg_ticket_price, na.rm = TRUE)
    avg_att <- mean(current_data()$rq3_event$total_attendance, na.rm = TRUE)

    paste0(
      "In the filtered sample, average F&B revenue per attendee is $", round(avg_rev, 2),
      ", average ticket price is $", round(avg_ticket, 2),
      ", and average attendance is ", comma(round(avg_att, 0)), "."
    )
  })

  # EDA
  output$eda_rev_ticket_plot <- renderPlot({
    ggplot(current_data()$rq3_event,
           aes(x = avg_ticket_price, y = fnb_rev_per_cap, color = is_playoff)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "F&B Revenue per Capita vs Average Ticket Price",
        x = "Average Ticket Price ($)",
        y = "F&B Revenue per Capita ($)",
        color = "Playoff"
      ) +
      theme_minimal()
  })

  output$eda_price_box_plot <- renderPlot({
    ggplot(current_data()$category_game, aes(x = category, y = avg_price, fill = category)) +
      geom_boxplot(alpha = 0.8) +
      labs(
        title = "Average Price Distribution by Category",
        x = "Category",
        y = "Average Category Price ($)"
      ) +
      theme_minimal() +
      theme(legend.position = "none", axis.text.x = element_text(angle = 40, hjust = 1))
  })

  output$eda_category_rev_plot <- renderPlot({
    summary_df <- current_data()$category_game %>%
      group_by(category) %>%
      summarise(rev_per_capita = mean(rev_per_capita, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(rev_per_capita))

    ggplot(summary_df, aes(x = reorder(category, rev_per_capita), y = rev_per_capita, fill = category)) +
      geom_col(alpha = 0.85) +
      coord_flip() +
      labs(
        title = "Average Revenue per Capita by Category",
        x = "Category",
        y = "Revenue per Capita ($)"
      ) +
      theme_minimal() +
      theme(legend.position = "none")
  })

  output$eda_time_plot <- renderPlot({
    ggplot(current_data()$rq3_event, aes(x = event_date, y = fnb_rev_per_cap, color = season_year)) +
      geom_line(alpha = 0.7) +
      geom_point(size = 1.8) +
      labs(
        title = "F&B Revenue per Capita Over Time",
        x = "Event Date",
        y = "F&B Revenue per Capita ($)",
        color = "Season"
      ) +
      theme_minimal()
  })

  # RQ1
  output$rq1_stats <- renderTable({
    model <- rq1_model()
    beta <- coef(model)["log_avg_price"]
    glance_df <- broom::glance(model)
    data.frame(
      Metric = c("Games", "Elasticity", "Elasticity type", "R-squared", "Adj. R-squared", "RMSE (response)", "MAPE (%)"),
      Value = c(
        nrow(rq1_data()),
        round(beta, 3),
        elasticity_label(beta),
        round(glance_df$r.squared, 3),
        round(glance_df$adj.r.squared, 3),
        round(rmse(exp(model$model$log_qty_per_capita), exp(fitted(model))), 4),
        round(mape(exp(model$model$log_qty_per_capita), exp(fitted(model))), 2)
      )
    )
  })

  output$rq1_scatter <- renderPlot({
    ggplot(rq1_data(), aes(x = avg_price, y = qty_per_capita, color = is_playoff)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = paste("Price vs Quantity per Capita -", input$rq1_category),
        x = "Average Price ($)",
        y = "Quantity per Capita",
        color = "Playoff"
      ) +
      theme_minimal()
  })

  output$rq1_actual_fitted <- renderPlot({
    d <- rq1_data() %>% mutate(fitted = exp(fitted(rq1_model())))
    ggplot(d, aes(x = qty_per_capita, y = fitted)) +
      geom_point(alpha = 0.75) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Actual vs Fitted Quantity per Capita",
        x = "Actual",
        y = "Fitted"
      ) +
      theme_minimal()
  })

  output$rq1_resid_plot <- renderPlot({
    aug <- broom::augment(rq1_model())
    ggplot(aug, aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.75) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(
        title = "Residuals vs Fitted",
        x = "Fitted values",
        y = "Residuals"
      ) +
      theme_minimal()
  })

  output$rq1_qq_plot <- renderPlot({
    aug <- broom::augment(rq1_model())
    ggplot(aug, aes(sample = .std.resid)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
      theme_minimal()
  })

  output$rq1_prediction_table <- renderTable({
    d <- rq1_data()
    base_price <- mean(d$avg_price, na.rm = TRUE)
    scenario_price <- base_price * (1 + input$rq1_price_change / 100)

    scn_attendance <- isolate(input$global_attendance_scn)
    scn_ticket     <- isolate(input$rq3_ticket_price)
    scn_playoff    <- isolate(input$global_playoff_scn)
    scn_weekend    <- isolate(input$global_weekend_scn)
    scn_season     <- isolate(input$global_season_scn)

    newdata <- tibble(
      log_avg_price = log(scenario_price),
      log_attendance = log(scn_attendance),
      log_avg_ticket_price = log(scn_ticket),
      is_playoff = factor(as.integer(scn_playoff), levels = c(0, 1)),
      is_weekend = factor(as.integer(scn_weekend), levels = c(0, 1)),
      season_year = factor(scn_season, levels = all_seasons),
      season_tercile = factor("Middle", levels = all_terciles)
    )

    pred <- predict_backtransform(rq1_model(), newdata)
    qty_pc <- pred$fit_response[1]
    total_qty <- qty_pc * scn_attendance
    total_rev <- total_qty * scenario_price
    beta <- coef(rq1_model())["log_avg_price"]

    data.frame(
      Measure = c(
        "Baseline average price",
        "Scenario price",
        "Predicted quantity per capita",
        "Predicted total quantity",
        "Predicted total revenue",
        "Revenue change from elasticity formula (%)"
      ),
      Value = c(
        dollar(base_price),
        dollar(scenario_price),
        round(qty_pc, 4),
        comma(round(total_qty, 0)),
        dollar(total_rev),
        round(revenue_change_pct(input$rq1_price_change, beta), 2)
      )
    )
  })

  output$rq1_text <- renderText({
    beta <- coef(rq1_model())["log_avg_price"]
    q_change <- round(beta * input$rq1_price_change, 1)
    r_change <- round(revenue_change_pct(input$rq1_price_change, beta), 1)
    paste0(
      "For ", input$rq1_category, ", the estimated category-level price coefficient is ", round(beta, 3),
      ". Under a ", input$rq1_price_change, "% price change scenario, quantity is expected to change by about ",
      q_change, "% and revenue by about ", r_change,
      "% using the elasticity-based revenue approximation. Positive estimates should be interpreted cautiously because high-demand games can push both price and sales upward."
    )
  })

  output$rq1_model_output <- renderPrint({
    summary(rq1_model())
  })

  # RQ2
  output$rq2_stats <- renderTable({
    model <- rq2_model()
    beta <- coef(model)["log_avg_price"]
    item_df <- rq2_data()
    data.frame(
      Metric = c("Games", "Category", "Elasticity", "Elasticity type", "R-squared", "RMSE (response)", "MAPE (%)"),
      Value = c(
        nrow(item_df),
        unique(item_df$category)[1],
        round(beta, 3),
        elasticity_label(beta),
        round(summary(model)$r.squared, 3),
        round(rmse(exp(model$model$log_qty_per_capita), exp(fitted(model))), 4),
        round(mape(exp(model$model$log_qty_per_capita), exp(fitted(model))), 2)
      )
    )
  })

  output$rq2_scatter <- renderPlot({
    ggplot(rq2_data(), aes(x = avg_price, y = qty_per_capita, color = season_year)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = paste("Item Price vs Quantity per Capita -", input$rq2_item),
        x = "Average Price ($)",
        y = "Quantity per Capita",
        color = "Season"
      ) +
      theme_minimal()
  })

  output$rq2_price_time <- renderPlot({
    ggplot(rq2_data() %>% arrange(event_date), aes(x = event_date, y = avg_price, color = season_year)) +
      geom_line(alpha = 0.75) +
      geom_point(size = 1.8) +
      labs(
        title = paste("Item Price Over Time -", input$rq2_item),
        x = "Event Date",
        y = "Average Price ($)",
        color = "Season"
      ) +
      theme_minimal()
  })

  output$rq2_actual_fitted <- renderPlot({
    d <- rq2_data() %>% mutate(fitted = exp(fitted(rq2_model())))
    ggplot(d, aes(x = qty_per_capita, y = fitted)) +
      geom_point(alpha = 0.75) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Actual vs Fitted Quantity per Capita", x = "Actual", y = "Fitted") +
      theme_minimal()
  })

  output$rq2_resid_plot <- renderPlot({
    aug <- broom::augment(rq2_model())
    ggplot(aug, aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.75) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
      theme_minimal()
  })

  output$rq2_qq_plot <- renderPlot({
    aug <- broom::augment(rq2_model())
    ggplot(aug, aes(sample = .std.resid)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
      theme_minimal()
  })

  output$rq2_prediction_table <- renderTable({
    d <- rq2_data()
    base_price <- mean(d$avg_price, na.rm = TRUE)
    scenario_price <- base_price * (1 + input$rq2_price_change / 100)

    scn_attendance <- isolate(input$global_attendance_scn)
    scn_ticket     <- isolate(input$rq3_ticket_price)
    scn_playoff    <- isolate(input$global_playoff_scn)
    scn_weekend    <- isolate(input$global_weekend_scn)
    scn_season     <- isolate(input$global_season_scn)

    newdata <- tibble(
      log_avg_price = log(scenario_price),
      log_attendance = log(scn_attendance),
      log_avg_ticket_price = log(scn_ticket),
      is_playoff = factor(as.integer(scn_playoff), levels = c(0, 1)),
      is_weekend = factor(as.integer(scn_weekend), levels = c(0, 1)),
      season_year = factor(scn_season, levels = all_seasons),
      season_tercile = factor("Middle", levels = all_terciles)
    )

    pred <- predict_backtransform(rq2_model(), newdata)
    qty_pc <- pred$fit_response[1]
    total_qty <- qty_pc * scn_attendance
    total_rev <- total_qty * scenario_price
    beta <- coef(rq2_model())["log_avg_price"]

    data.frame(
      Measure = c(
        "Baseline average price",
        "Scenario price",
        "Predicted quantity per capita",
        "Predicted total quantity",
        "Predicted total revenue",
        "Revenue change from elasticity formula (%)"
      ),
      Value = c(
        dollar(base_price),
        dollar(scenario_price),
        round(qty_pc, 5),
        comma(round(total_qty, 0)),
        dollar(total_rev),
        round(revenue_change_pct(input$rq2_price_change, beta), 2)
      )
    )
  })

  output$rq2_text <- renderText({
    beta <- coef(rq2_model())["log_avg_price"]
    q_change <- round(beta * input$rq2_price_change, 1)
    r_change <- round(revenue_change_pct(input$rq2_price_change, beta), 1)
    paste0(
      "For item ", input$rq2_item, ", the estimated item-level price coefficient is ", round(beta, 3),
      ". Under a ", input$rq2_price_change, "% price change scenario, quantity is expected to change by about ",
      q_change, "% and revenue by about ", r_change,
      "%. This item-level model is most useful for items with real price movement across games and across seasons."
    )
  })

  output$rq2_model_output <- renderPrint({
    summary(rq2_model())
  })

  # RQ3
  output$rq3_stats <- renderTable({
    model <- rq3_model()
    beta_ticket <- coef(model)["log_avg_ticket_price"]
    glance_df <- broom::glance(model)
    data.frame(
      Metric = c("Games", "Ticket price coefficient", "R-squared", "Adj. R-squared", "RMSE (response)", "MAPE (%)"),
      Value = c(
        nrow(current_data()$rq3_event),
        round(beta_ticket, 3),
        round(glance_df$r.squared, 3),
        round(glance_df$adj.r.squared, 3),
        round(rmse(exp(model$model$log_fnb_rev_per_cap), exp(fitted(model))), 4),
        round(mape(exp(model$model$log_fnb_rev_per_cap), exp(fitted(model))), 2)
      )
    )
  })

  output$rq3_scatter <- renderPlot({
    ggplot(current_data()$rq3_event, aes(x = avg_ticket_price, y = fnb_rev_per_cap, color = is_playoff)) +
      geom_point(alpha = 0.75, size = 2) +
      geom_smooth(method = "lm", se = FALSE) +
      labs(
        title = "Average Ticket Price vs F&B Revenue per Capita",
        x = "Average Ticket Price ($)",
        y = "F&B Revenue per Capita ($)",
        color = "Playoff"
      ) +
      theme_minimal()
  })

  output$rq3_coef_plot <- renderPlot({
    coef_df <- broom::tidy(rq3_model(), conf.int = TRUE) %>%
      filter(term != "(Intercept)")

    ggplot(coef_df, aes(x = estimate, y = reorder(term, estimate))) +
      geom_point() +
      geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Coefficient Plot", x = "Estimate", y = "Predictor") +
      theme_minimal()
  })

  output$rq3_actual_fitted <- renderPlot({
    d <- current_data()$rq3_event %>% mutate(fitted = exp(fitted(rq3_model())))
    ggplot(d, aes(x = fnb_rev_per_cap, y = fitted)) +
      geom_point(alpha = 0.75) +
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Actual vs Fitted F&B Revenue per Capita", x = "Actual", y = "Fitted") +
      theme_minimal()
  })

  output$rq3_resid_plot <- renderPlot({
    aug <- broom::augment(rq3_model())
    ggplot(aug, aes(x = .fitted, y = .resid)) +
      geom_point(alpha = 0.75) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
      labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
      theme_minimal()
  })

  output$rq3_qq_plot <- renderPlot({
    aug <- broom::augment(rq3_model())
    ggplot(aug, aes(sample = .std.resid)) +
      stat_qq() +
      stat_qq_line() +
      labs(title = "Normal Q-Q Plot", x = "Theoretical Quantiles", y = "Standardized Residuals") +
      theme_minimal()
  })

  output$rq3_prediction_table <- renderTable({
    base_ticket <- mean(current_data()$rq3_event$avg_ticket_price, na.rm = TRUE)

    scn_attendance <- isolate(input$global_attendance_scn)
    scn_playoff    <- isolate(input$global_playoff_scn)
    scn_weekend    <- isolate(input$global_weekend_scn)
    scn_season     <- isolate(input$global_season_scn)
    scn_ticket     <- isolate(input$rq3_ticket_price)
    scn_ticket_chg <- isolate(input$rq3_ticket_change)

    scenario_ticket <- scn_ticket * (1 + scn_ticket_chg / 100)

    newdata <- tibble(
      log_avg_ticket_price = log(scenario_ticket),
      log_attendance = log(scn_attendance),
      is_weekend = factor(as.integer(scn_weekend), levels = c(0, 1)),
      is_playoff = factor(as.integer(scn_playoff), levels = c(0, 1)),
      season_tercile = factor("Middle", levels = all_terciles),
      season_year = factor(scn_season, levels = all_seasons)
    )

    pred <- predict_backtransform(rq3_model(), newdata)
    rev_pc <- pred$fit_response[1]
    total_rev <- rev_pc * scn_attendance
    beta <- coef(rq3_model())["log_avg_ticket_price"]

    data.frame(
      Measure = c(
        "Average ticket price baseline",
        "Scenario ticket price",
        "Predicted F&B revenue per capita",
        "Predicted total F&B revenue",
        "Expected per-capita change from ticket coefficient (%)"
      ),
      Value = c(
        dollar(base_ticket),
        dollar(scenario_ticket),
        dollar(rev_pc),
        dollar(total_rev),
        round(beta * scn_ticket_chg, 2)
      )
    )
  })

  output$rq3_text <- renderText({
    beta <- coef(rq3_model())["log_avg_ticket_price"]
    chg  <- isolate(input$rq3_ticket_change)
    implied <- round(beta * chg, 2)
    paste0(
      "The event-level ticket price coefficient is ", round(beta, 3),
      ". In this model, a ", chg,
      "% ticket price change is associated with about ", implied,
      "% change in F&B revenue per attendee, holding the other included factors constant."
    )
  })

  output$rq3_model_output <- renderPrint({
    summary(rq3_model())
  })

  # Compare categories
  compare_df <- reactive({
    cats <- sort(unique(current_data()$category_game$category))
    bind_rows(lapply(cats, function(cat_nm) {
      d <- current_data()$category_game %>% filter(category == cat_nm)
      if (nrow(d) < 8) return(NULL)
      model <- tryCatch({
        f <- build_formula(
          "log_qty_per_capita", d,
          c("log_avg_price", "log_attendance", "log_avg_ticket_price",
            "is_playoff", "is_weekend", "season_year", "season_tercile")
        )
        lm(f, data = d)
      }, error = function(e) NULL)
      if (is.null(model)) return(NULL)
      coef_df <- broom::tidy(model, conf.int = TRUE)
      row <- coef_df %>% filter(term == "log_avg_price")
      if (nrow(row) == 0) return(NULL)
      tibble(
        category = cat_nm,
        elasticity = row$estimate,
        conf_low = row$conf.low,
        conf_high = row$conf.high,
        p_value = row$p.value,
        r_squared = summary(model)$r.squared,
        n_games = nrow(d)
      )
    }))
  })

  output$compare_plot <- renderPlot({
    d <- compare_df()
    ggplot(d, aes(x = elasticity, y = reorder(category, elasticity))) +
      geom_point(size = 2) +
      geom_errorbarh(aes(xmin = conf_low, xmax = conf_high), height = 0.2) +
      geom_vline(xintercept = 0, linetype = "dashed") +
      geom_vline(xintercept = -1, linetype = "dashed", color = "red") +
      labs(title = "Category Elasticity Comparison", x = "Elasticity Estimate", y = "Category") +
      theme_minimal()
  })

  output$compare_table <- renderTable({
    compare_df() %>%
      mutate(
        elasticity = round(elasticity, 3),
        conf_low = round(conf_low, 3),
        conf_high = round(conf_high, 3),
        p_value = round(p_value, 4),
        r_squared = round(r_squared, 3)
      ) %>%
      arrange(elasticity)
  })

  # Data tab
  current_table <- reactive({
    if (input$data_view == "cat") {
      current_data()$category_game %>%
        select(event_date, category, total_qty, avg_price, total_revenue,
               qty_per_capita, rev_per_capita, total_attendance,
               avg_ticket_price, is_playoff, is_weekend,
               season_year, season_tercile)
    } else if (input$data_view == "item") {
      current_data()$item_game %>%
        select(event_date, item_name, category, total_qty, avg_price, total_revenue,
               qty_per_capita, rev_per_capita, total_attendance,
               avg_ticket_price, is_playoff, is_weekend,
               season_year, season_tercile)
    } else {
      current_data()$rq3_event %>%
        select(event_date, total_attendance, avg_ticket_price,
               event_fnb_revenue, fnb_rev_per_cap, fnb_qty_per_cap,
               is_playoff, is_weekend, season_year, season_tercile)
    }
  })

  output$data_table <- renderDT({
    datatable(current_table(), options = list(pageLength = 15, scrollX = TRUE))
  })

  output$download_data <- downloadHandler(
    filename = function() paste0("hurricanes_app_data_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(current_table(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui = ui, server = server)
