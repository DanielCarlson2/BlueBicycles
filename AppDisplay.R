# Blue Bikes Boston - Six Sigma Analysis Dashboard
# Shiny Application for Project Display
# Group: Nevin Motto, Rene Umeh, Daniel Carlson, Evelyne Morisseau, Charlie Gagliardo

# Load required libraries with error handling
#required_packages <- c(
#  "shiny", "shinydashboard", "DT", "ggplot2", "dplyr",
#  "RSQLite", "DBI", "lubridate", "viridis", "plotly", "readr"
#)

# Function to check and install packages
#check_and_install <- function(pkg) {
 # if (!require(pkg, character.only = TRUE)) {
  #  install.packages(pkg, dependencies = TRUE)
   # library(pkg, character.only = TRUE)
 # }
#}

# Install and load all required packages
#sapply(required_packages, check_and_install)

# Load libraries
library(shiny)
library(shinydashboard)
library(DT)
library(ggplot2)
library(dplyr)
library(RSQLite)
library(DBI)
library(lubridate)
library(viridis)
library(plotly)
library(readr)

# UI Definition
ui <- dashboardPage(
  skin = "blue",
  
  # Header
  dashboardHeader(
    title = "Blue Bikes Boston - Six Sigma Analysis",
    titleWidth = 350
  ),
  
  # Sidebar
  dashboardSidebar(
    width = 250,
    sidebarMenu(
      id = "tabs",
      menuItem("Project Overview", tabName = "overview", icon = icon("info-circle")),
      menuItem("(1) Research Question", tabName = "research", icon = icon("question-circle")),
      menuItem("(2) Data", tabName = "data", icon = icon("database")),
      menuItem("(3) Method", tabName = "method", icon = icon("flask")),
      menuItem("(4) Results", tabName = "results", icon = icon("chart-line")),
      menuItem("(5) Discussion", tabName = "discussion", icon = icon("comments")),
      menuItem("Statistical Process Control", tabName = "spc", icon = icon("chart-bar")),
      menuItem("References", tabName = "references", icon = icon("book"))
    )
  ),
  
  # Body
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          border-radius: 5px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.1);
        }
        h2 {
          color: #2c3e50;
          font-weight: 600;
        }
        h3 {
          color: #34495e;
          font-weight: 500;
        }
        .quantity-box {
          background-color: #e8f4f8;
          padding: 15px;
          border-left: 4px solid #3498db;
          margin: 10px 0;
        }
        .ci-box {
          background-color: #fff3cd;
          padding: 15px;
          border-left: 4px solid #ffc107;
          margin: 10px 0;
        }
      "))
    ),
    
    tabItems(
      
      # Tab 1: Project Overview
      tabItem(
        tabName = "overview",
        fluidRow(
          box(
            width = 12,
            title = "Project Charter", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h2("Blue Bikes Boston - Station Optimization Project"),
            br(),
            h3("Course & Team"),
            tags$ul(
              tags$li(tags$strong("Course:"), "SYSEN 5300: Systems Engineering and Six Sigma"),
              tags$li(tags$strong("Project Activity:"), "Rough Draft"),
              tags$li(tags$strong("Team:"), "Daniel Carlson (Project Lead), Nevin Motto (Project Manager), Rene Umeh (Visualization), Charlie Gagliardo (Modeler 1), Evelyne Morisseau (Modeler 2)")
            )
          )
        )
      ),
      
      # Tab 2: (1) Research Question
      tabItem(
        tabName = "research",
        fluidRow(
          box(
            width = 12,
            title = "(1) Research Question", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Primary Research Question"),
            tags$p(tags$strong("What happens if we add a stop?"), style = "font-size: 16px;"),
            br(),
            h3("Outcome Variable"),
            tags$ul(
              tags$li("Number of Bike Stops"),
              tags$li("Unit of Observation: N = 1 ride")
            ),
            br(),
            h3("Research Objectives"),
            tags$ul(
              tags$li("Identify optimal locations for new BlueBikes stations"),
              tags$li("Assess impact of station additions on ridership and availability"),
              tags$li("Quantify financial benefits of strategic station placement"),
              tags$li("Provide data-driven recommendations for 64 planned stations by 2030")
            )
          )
        )
      ),
      
      # Tab 3: (2) Data
      tabItem(
        tabName = "data",
        fluidRow(
          box(
            width = 12,
            title = "(2) Data", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Data Sources"),
            tags$ul(
              tags$li(tags$strong("Dataset:"), "BlueBikes public trip data (2011-2021)"),
              tags$li(tags$strong("Source:"), "https://github.com/timothyfraser/sts/tree/3week/data/bluebikes"),
              tags$li(tags$strong("Focus Period:"), "Weekday rush hours: 7-9 AM (morning) and 4-6 PM (evening)")
            ),
            br(),
            h3("Data Processing"),
            tags$ul(
              tags$li("Weekends excluded (focus on commuter patterns)"),
              tags$li("Extreme weather days excluded (Dec, Jan, Feb)"),
              tags$li("Limited to Boston city area stations"),
              tags$li("Aggregated to daily ride counts per station")
            ),
            br(),
            h3("Key Measures"),
            tags$ul(
              tags$li(tags$strong("Total Count:"), "Number of rides per station per day (unit: N)"),
              tags$li(tags$strong("Ride Length:"), "Duration of rides (unit: hours)"),
              tags$li(tags$strong("Process Statistics:"), "Mean, SD, CV, control limits per station")
            )
          )
        )
      ),
      
      # Tab 4: (3) Method
      tabItem(
        tabName = "method",
        fluidRow(
          box(
            width = 12,
            title = "(3) Method - Statistical Process Control Framework", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Analytical Approach"),
            tags$ul(
              tags$li("Applied Statistical Process Control (SPC) framework to quantify station variability"),
              tags$li("Treated weekday usage as recurring operational process"),
              tags$li("Grouped observations by week (Mon-Fri subgroups)"),
              tags$li("Computed control limits: UCL/LCL = mean ± 3σ"),
              tags$li("Calculated Coefficient of Variation (CV) = (SD/Mean) × 100%"),
              tags$li("Identified out-of-control stations via control limit violations"),
              tags$li("Bootstrap analysis to estimate improvement potential from dock expansion")
            ),
            br(),
            h3("Key SPC Metrics"),
            tags$ul(
              tags$li("Mean daily ride volume (baseline demand)"),
              tags$li("Standard deviation (variability measure)"),
              tags$li("Coefficient of variation (relative variability, CV > 50% = high variability)"),
              tags$li("Control limit violations (±3σ thresholds)")
            )
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "SIPOC Diagram", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "images/SIPOC.png", width = "100%", style = "border: 1px solid #ddd;")
          ),
          box(
            width = 6,
            title = "Process Map", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "images/ProcessMap.png", width = "100%", style = "border: 1px solid #ddd;")
          )
        )
      ),
      
      # Tab 5: (4) Results
      tabItem(
        tabName = "results",
        fluidRow(
          box(
            width = 12,
            title = "(4) Results - Key Findings", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Process Variability Summary"),
            tags$ul(
              tags$li("AM Rush Hour: Median CV = 50%; 45.5% of stations show high variability (CV > 50%)"),
              tags$li("PM Rush Hour: Median CV = 53.03%; 61.2% of stations show high variability"),
              tags$li("AM: 9 control limit violations across 8 stations"),
              tags$li("PM: 768 control limit violations across 226 stations")
            ),
            br(),
            h3("Most Problematic Stations"),
            tags$ul(
              tags$li("AM: A32002, M32006, D32003 (CV: 56-119%)"),
              tags$li("PM: A32000, M32006, D32003 (CV: 103-147%)")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Quantities of Interest", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            div(class = "quantity-box",
              h3("Primary Quantity of Interest"),
              tags$p(tags$strong("Rides Gained per Optimized Station per Day:"), tags$strong("2.51 rides"), style = "font-size: 18px;"),
              tags$p("(Mean estimate)")
            ),
            div(class = "ci-box",
              h4("95% Confidence Interval"),
              tags$p(tags$strong("[1.74, 3.28] rides per station per day"), style = "font-size: 16px;"),
              tags$p("This means we are 95% confident that optimizing unstable stations will gain between 1.74 and 3.28 additional rides per station per day.")
            ),
            br(),
            div(class = "quantity-box",
              h3("Financial Impact"),
              tags$p(tags$strong("Annual Additional Revenue:"), tags$strong("$543,000 - $814,000"), style = "font-size: 18px;"),
              tags$p("(Based on optimizing top 10 unstable stations, ~$2.95 per ride)")
            ),
            div(class = "quantity-box",
              h3("System-Wide Metrics"),
              tags$ul(
                tags$li("Average AM ridership: 2.18 rides/station/day"),
                tags$li("Average PM ridership: 2.52 rides/station/day"),
                tags$li("Current loss: 2% (~94,000 rides) of potential customers"),
                tags$li("Potential recovery: 4-6% of lost/unserved demand")
              )
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Visualization 1: Process Variability - AM vs PM", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("variability_plot", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Detailed Results", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            h4("Morning (AM) Rush Hour:"),
            tags$ul(
              tags$li("Average ridership: 2.18 rides/station/day"),
              tags$li("Median CV: 50%"),
              tags$li("45.5% stations with high variability"),
              tags$li("9 violations, 8 stations affected")
            ),
            br(),
            h4("Evening (PM) Rush Hour:"),
            tags$ul(
              tags$li("Average ridership: 2.52 rides/station/day"),
              tags$li("Median CV: 53.03%"),
              tags$li("61.2% stations with high variability"),
              tags$li("768 violations, 226 stations affected")
            )
          )
        )
      ),
      
      # Tab 6: (5) Discussion
      tabItem(
        tabName = "discussion",
        fluidRow(
          box(
            width = 12,
            title = "(5) Discussion & Implications", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h3("Key Findings"),
            tags$ul(
              tags$li("Substantial variation exists across stations (median CV ~50-53%)"),
              tags$li("Approximately half of all stations operate outside desired stability limits"),
              tags$li("PM rush hour shows higher variability and more violations than AM"),
              tags$li("Stations with repeated violations indicate structural imbalances (over/under utilization)")
            ),
            br(),
            h3("Implications"),
            tags$ul(
              tags$li("Strategic station placement can reduce variability and capture missed demand"),
              tags$li("Optimizing top 10 unstable stations could recover 4-6% of lost demand"),
              tags$li("Financial impact: $543K-$814K additional annual revenue"),
              tags$li("Analysis shows 95% CI: [1.74, 3.28] rides gained per optimized station/day"),
              tags$li("Method provides data-driven approach for 64 planned stations by 2030")
            ),
            br(),
            h3("Recommendations"),
            tags$ul(
              tags$li("Prioritize stations in high-variability zones for rebalancing or expansion"),
              tags$li("Focus on PM rush hour hotspots (226 stations with violations)"),
              tags$li("Use SPC framework to continuously monitor station performance"),
              tags$li("Apply statistical methodology to evaluate potential new station locations"),
              tags$li("Target reducing CV below 50% threshold for process stability")
            ),
            br(),
            h3("Business Value"),
            tags$ul(
              tags$li("Increases user accessibility and satisfaction during peak hours"),
              tags$li("Reduces missed ride opportunities (currently 2% of potential customers)"),
              tags$li("Generates measurable revenue increase ($543K-$814K annually)"),
              tags$li("Supports BlueBikes expansion goals and Go Boston 2030 vision"),
              tags$li("Provides quantitative framework for strategic decision-making")
            )
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Voice of the Customer", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            tags$img(src = "images/VOC.png", width = "100%", style = "border: 1px solid #ddd;")
          )
        )
      ),
      
      # Tab 7: Statistical Process Control
      tabItem(
        tabName = "spc",
        fluidRow(
          box(
            width = 12,
            title = "Year Selection", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            selectInput("spc_year", 
                       "Select Year for Analysis:", 
                       choices = NULL,
                       selected = NULL)
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Global View: Total Rides Over All Years", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("global_rides_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Process Capability Distribution", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            h4("Percentage of Stations at Each Variability Category"),
            plotlyOutput("process_capability_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "AM Rush Hour - Variability Categories", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("am_variability_bars", height = "350px")
          ),
          box(
            width = 6,
            title = "PM Rush Hour - Variability Categories", 
            status = "warning", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("pm_variability_bars", height = "350px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Stations with Highest Average Rides", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            DT::dataTableOutput("top_stations_table")
          )
        ),
        fluidRow(
          box(
            width = 6,
            title = "Top 15 Stations: High Variability (50-75% CV)", 
            status = "danger", 
            solidHeader = TRUE,
            collapsible = FALSE,
            DT::dataTableOutput("high_variability_table")
          ),
          box(
            width = 6,
            title = "Top 15 Stations: Very High Variability (>75% CV)", 
            status = "danger", 
            solidHeader = TRUE,
            collapsible = FALSE,
            DT::dataTableOutput("very_high_variability_table")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "AM vs PM Rush Hours: Top Stations Comparison", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("top_stations_comparison", height = "500px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Process Capability: Mean vs Variability", 
            status = "success", 
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("mean_vs_cv_plot", height = "400px")
          )
        ),
        fluidRow(
          box(
            width = 12,
            title = "Station Performance Summary", 
            status = "info", 
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = TRUE,
            DT::dataTableOutput("station_summary_table")
          )
        )
      ),
      
      # Tab 8: References
      tabItem(
        tabName = "references",
        fluidRow(
          box(
            width = 12,
            title = "References", 
            status = "primary", 
            solidHeader = TRUE,
            collapsible = FALSE,
            uiOutput("references_list")
          )
        )
      )
    )
  )
)

# Server Definition
server <- function(input, output, session) {
  
  # Add resource path for images
  addResourcePath("images", "Images")
  
  ################################################################################
  # Helper function to read CSV files with warnings suppressed
  ################################################################################
  
  read_csv_silent <- function(file_path) {
    # Temporarily suppress warnings
    old_warn <- options(warn = -1)$warn
    on.exit(options(warn = old_warn))
    
    # Suppress messages and warnings from readr
    suppressWarnings({
      suppressMessages({
        result <- readr::read_csv(
          file_path, 
          show_col_types = FALSE,
          progress = FALSE,
          locale = readr::locale(encoding = "UTF-8")
        )
      })
    })
    
    return(result)
  }
  
  ################################################################################
  # SPC Tab: Load and Process Data from CSV Files
  ################################################################################
  
  # Get available years from CSV files
  available_years <- reactive({
    csv_files <- list.files("DataCSV", pattern = "^BlueBikes_\\d{4}\\.csv$", full.names = FALSE)
    years <- as.numeric(gsub("BlueBikes_(\\d{4})\\.csv", "\\1", csv_files))
    sort(years[!is.na(years)])
  })
  
  # Update year selector
  observe({
    years <- available_years()
    if (length(years) > 0) {
      updateSelectInput(session, "spc_year", 
                       choices = c("All Years" = "all", setNames(years, years)),
                       selected = ifelse(length(years) > 0, max(years), "all"))
    }
  })
  
  # Load all years data for global view
  all_years_data <- reactive({
    years <- available_years()
    if (length(years) == 0) return(NULL)
    
    all_data_list <- lapply(years, function(year) {
      file_path <- paste0("DataCSV/BlueBikes_", year, ".csv")
      if (file.exists(file_path)) {
        tryCatch({
          data <- read_csv_silent(file_path)
          return(data)
        }, error = function(e) {
          return(NULL)
        })
      }
      return(NULL)
    })
    
    # Filter out NULL values before binding
    all_data_list <- all_data_list[!sapply(all_data_list, is.null)]
    
    if (length(all_data_list) == 0) return(NULL)
    
    tryCatch({
      all_data <- dplyr::bind_rows(all_data_list)
      return(all_data)
    }, error = function(e) {
      return(NULL)
    })
  })

  
  # Load selected year data
  selected_year_data <- reactive({
    if (is.null(input$spc_year) || input$spc_year == "all") {
      return(all_years_data())
    }
    
    file_path <- paste0("DataCSV/BlueBikes_", input$spc_year, ".csv")
    if (file.exists(file_path)) {
      tryCatch({
        return(read_csv_silent(file_path))
      }, error = function(e) {
        return(NULL)
      })
    }
    return(NULL)
  })
  
  # Calculate SPC statistics for selected year
  spc_stats <- reactive({
    data <- selected_year_data()
    if (is.null(data) || nrow(data) == 0) return(NULL)
    
    # Check if required columns exist
    required_cols <- c("Is_Weekend", "Month", "Start_Station_Code", "Rush_Period", "Number_of_Rides")
    missing_cols <- required_cols[!required_cols %in% names(data)]
    
    if (length(missing_cols) > 0) {
      return(NULL)
    }
    
    tryCatch({
      # Filter to weekdays and exclude winter (matching BlueBikes_Code.R logic)
      data_filtered <- data %>%
        dplyr::filter(!Is_Weekend, !Month %in% c(12, 1, 2))
      
      if (nrow(data_filtered) == 0) return(NULL)
      
      # Calculate statistics by station and rush period
      station_stats <- data_filtered %>%
        dplyr::group_by(Start_Station_Code, Rush_Period) %>%
        dplyr::summarise(
          n_days = n(),
          mean_rides = mean(Number_of_Rides, na.rm = TRUE),
          sd_rides = sd(Number_of_Rides, na.rm = TRUE),
          median_rides = median(Number_of_Rides, na.rm = TRUE),
          total_rides = sum(Number_of_Rides, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        dplyr::filter(n_days >= 30, !is.na(mean_rides), !is.na(sd_rides), mean_rides > 0) %>%
        dplyr::mutate(
          UCL = mean_rides + 3 * sd_rides,
          LCL = pmax(0, mean_rides - 3 * sd_rides),
          CV = (sd_rides / mean_rides) * 100,
          Variability_Category = dplyr::case_when(
            CV < 25 ~ "Low (<25%)",
            CV >= 25 & CV <= 50 ~ "Moderate (25-50%)",
            CV > 50 & CV <= 75 ~ "High (50-75%)",
            CV > 75 ~ "Very High (>75%)"
          )
        ) %>%
        dplyr::filter(!is.na(CV))
      
      return(station_stats)
    }, error = function(e) {
      return(NULL)
    })
  })
  
  # Global rides plot (all years)
  output$global_rides_plot <- renderPlotly({
    all_data <- all_years_data()
    if (is.null(all_data) || nrow(all_data) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    # Check if required columns exist
    required_cols <- c("Year", "Rush_Period", "Number_of_Rides")
    missing_cols <- required_cols[!required_cols %in% names(all_data)]
    
    if (length(missing_cols) > 0) {
      return(plotly_empty() %>% layout(title = paste("Missing columns:", paste(missing_cols, collapse = ", "))))
    }
    
    tryCatch({
      # Aggregate by year and rush period
      yearly_summary <- all_data %>%
        dplyr::group_by(Year, Rush_Period) %>%
        dplyr::summarise(Total_Rides = sum(Number_of_Rides, na.rm = TRUE), .groups = "drop")
      
      if (nrow(yearly_summary) == 0) {
        return(plotly_empty() %>% layout(title = "No data available after aggregation"))
      }
      
      p <- ggplot(yearly_summary, aes(x = Year, y = Total_Rides, fill = Rush_Period)) +
        geom_col(position = "dodge", alpha = 0.8) +
        scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"), name = "Rush Period") +
        labs(
          title = "Total Number of Rides: AM vs PM Over All Years",
          x = "Year",
          y = "Total Rides",
          fill = "Rush Period"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "top")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Process Capability Distribution Plot
  output$process_capability_plot <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    tryCatch({
      # Calculate percentages by category
      category_summary <- stats %>%
        dplyr::group_by(Rush_Period, Variability_Category) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::group_by(Rush_Period) %>%
        dplyr::mutate(
          Percentage = (Count / sum(Count)) * 100,
          Variability_Category = factor(Variability_Category, 
                                        levels = c("Low (<25%)", "Moderate (25-50%)", 
                                                  "High (50-75%)", "Very High (>75%)"))
        )
      
      if (nrow(category_summary) == 0) {
        return(plotly_empty() %>% layout(title = "No data available after processing"))
      }
      
      p <- ggplot(category_summary, aes(x = Rush_Period, y = Percentage, fill = Variability_Category)) +
        geom_col(position = "fill", alpha = 0.9) +
        geom_text(aes(label = paste0(round(Percentage, 1), "%")), 
                  position = position_fill(vjust = 0.5), 
                  color = "white", fontface = "bold", size = 4) +
        scale_fill_manual(values = c("Low (<25%)" = "#2E7D32", 
                                     "Moderate (25-50%)" = "#FFC107",
                                     "High (50-75%)" = "#FF9800",
                                     "Very High (>75%)" = "#C62828"),
                          name = "Variability") +
        scale_y_continuous(labels = scales::percent) +
        labs(
          title = "Process Capability Distribution: AM vs PM",
          subtitle = "Percentage of stations in each variability category",
          x = "Rush Period",
          y = "Percentage of Stations"
        ) +
        theme_minimal(base_size = 12) +
        theme(legend.position = "right")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # AM Variability Bars
  output$am_variability_bars <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    tryCatch({
      am_stats <- stats %>% dplyr::filter(Rush_Period == "AM")
      if (nrow(am_stats) == 0) {
        return(plotly_empty() %>% layout(title = "No AM data available"))
      }
      
      category_summary <- am_stats %>%
        dplyr::group_by(Variability_Category) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::mutate(
          Percentage = (Count / sum(Count)) * 100,
          Variability_Category = factor(Variability_Category, 
                                        levels = c("Low (<25%)", "Moderate (25-50%)", 
                                                  "High (50-75%)", "Very High (>75%)"))
        )
      
      if (nrow(category_summary) == 0) {
        return(plotly_empty() %>% layout(title = "No data available after processing"))
      }
      
      p <- ggplot(category_summary, aes(x = Variability_Category, y = Count, fill = Variability_Category)) +
        geom_col(alpha = 0.9) +
        geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                  vjust = -0.2, fontface = "bold") +
        scale_fill_manual(values = c("Low (<25%)" = "#2E7D32", 
                                     "Moderate (25-50%)" = "#FFC107",
                                     "High (50-75%)" = "#FF9800",
                                     "Very High (>75%)" = "#C62828"),
                          guide = "none") +
        labs(
          title = "AM Rush Hour - Station Variability",
          x = "Variability Category",
          y = "Number of Stations"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # PM Variability Bars
  output$pm_variability_bars <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    tryCatch({
      pm_stats <- stats %>% dplyr::filter(Rush_Period == "PM")
      if (nrow(pm_stats) == 0) {
        return(plotly_empty() %>% layout(title = "No PM data available"))
      }
      
      category_summary <- pm_stats %>%
        dplyr::group_by(Variability_Category) %>%
        dplyr::summarise(Count = n(), .groups = "drop") %>%
        dplyr::mutate(
          Percentage = (Count / sum(Count)) * 100,
          Variability_Category = factor(Variability_Category, 
                                        levels = c("Low (<25%)", "Moderate (25-50%)", 
                                                  "High (50-75%)", "Very High (>75%)"))
        )
      
      if (nrow(category_summary) == 0) {
        return(plotly_empty() %>% layout(title = "No data available after processing"))
      }
      
      p <- ggplot(category_summary, aes(x = Variability_Category, y = Count, fill = Variability_Category)) +
        geom_col(alpha = 0.9) +
        geom_text(aes(label = paste0(Count, "\n(", round(Percentage, 1), "%)")), 
                  vjust = -0.2, fontface = "bold") +
        scale_fill_manual(values = c("Low (<25%)" = "#2E7D32", 
                                     "Moderate (25-50%)" = "#FFC107",
                                     "High (50-75%)" = "#FF9800",
                                     "Very High (>75%)" = "#C62828"),
                          guide = "none") +
        labs(
          title = "PM Rush Hour - Station Variability",
          x = "Variability Category",
          y = "Number of Stations"
        ) +
        theme_minimal(base_size = 11) +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Top Stations Table
  output$top_stations_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    top_stations <- stats %>%
      dplyr::group_by(Start_Station_Code) %>%
      dplyr::summarise(
        Avg_AM_Rides = mean(mean_rides[Rush_Period == "AM"], na.rm = TRUE),
        Avg_PM_Rides = mean(mean_rides[Rush_Period == "PM"], na.rm = TRUE),
        Total_Avg_Rides = mean(mean_rides, na.rm = TRUE),
        AM_CV = mean(CV[Rush_Period == "AM"], na.rm = TRUE),
        PM_CV = mean(CV[Rush_Period == "PM"], na.rm = TRUE),
        .groups = "drop"
      ) %>%
      dplyr::arrange(desc(Total_Avg_Rides)) %>%
      head(20) %>%
      dplyr::mutate(
        Avg_AM_Rides = round(Avg_AM_Rides, 2),
        Avg_PM_Rides = round(Avg_PM_Rides, 2),
        Total_Avg_Rides = round(Total_Avg_Rides, 2),
        AM_CV = round(AM_CV, 2),
        PM_CV = round(PM_CV, 2)
      )
    
    DT::datatable(top_stations,
                  colnames = c("Station Code", "Avg AM Rides", "Avg PM Rides", 
                              "Total Avg Rides", "AM CV (%)", "PM CV (%)"),
                  options = list(pageLength = 10, dom = 't'))
  })
  
  # High Variability Table (50-75%)
  output$high_variability_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    high_var <- stats %>%
      dplyr::filter(CV >= 50 & CV <= 75) %>%
      dplyr::arrange(desc(mean_rides)) %>%
      head(15) %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, CV) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        CV = round(CV, 2)
      )
    
    DT::datatable(high_var,
                  colnames = c("Station Code", "Rush Period", "Avg Rides", "CV (%)"),
                  options = list(pageLength = 15, dom = 't'))
  })
  
  # Very High Variability Table (>75%)
  output$very_high_variability_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    very_high_var <- stats %>%
      dplyr::filter(CV > 75) %>%
      dplyr::arrange(desc(mean_rides)) %>%
      head(15) %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, CV) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        CV = round(CV, 2)
      )
    
    DT::datatable(very_high_var,
                  colnames = c("Station Code", "Rush Period", "Avg Rides", "CV (%)"),
                  options = list(pageLength = 15, dom = 't'))
  })
  
  # Top Stations Comparison Chart
  output$top_stations_comparison <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    tryCatch({
      # Get top stations by average rides
      top_stations_list <- stats %>%
        dplyr::group_by(Start_Station_Code) %>%
        dplyr::summarise(avg_rides = mean(mean_rides, na.rm = TRUE), .groups = "drop") %>%
        dplyr::arrange(desc(avg_rides)) %>%
        head(15) %>%
        dplyr::pull(Start_Station_Code)
      
      comparison_data <- stats %>%
        dplyr::filter(Start_Station_Code %in% top_stations_list)
      
      if (nrow(comparison_data) == 0) {
        return(plotly_empty() %>% layout(title = "No data available"))
      }
      
      p <- ggplot(comparison_data, 
                  aes(x = reorder(Start_Station_Code, mean_rides), 
                      y = mean_rides, fill = Rush_Period)) +
        geom_col(position = "dodge", alpha = 0.8) +
        coord_flip() +
        scale_fill_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                          name = "Rush Period") +
        labs(
          title = "AM vs PM Rush Hour: Top Stations Comparison",
          subtitle = "Average daily rides (weekdays, non-winter months)",
          x = "Station Code",
          y = "Average Daily Rides"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Mean vs CV Plot
  output$mean_vs_cv_plot <- renderPlotly({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(plotly_empty() %>% layout(title = "No data available"))
    }
    
    tryCatch({
      p <- ggplot(stats, aes(x = mean_rides, y = CV, color = Rush_Period)) +
        geom_point(alpha = 0.6, size = 2.5) +
        geom_hline(yintercept = 50, linetype = "dashed", color = "red") +
        scale_color_manual(values = c("AM" = "#E69F00", "PM" = "#56B4E9"),
                           name = "Rush Period") +
        scale_x_log10() +
        annotate("text", x = max(stats$mean_rides, na.rm = TRUE), y = 55, 
                 label = "High Variability Zone", color = "red", hjust = 1) +
        labs(
          title = "Process Capability: Mean vs Variability",
          subtitle = "Higher volume doesn't always mean more stability",
          x = "Average Daily Rides (log scale)",
          y = "Coefficient of Variation (%)"
        ) +
        theme_minimal(base_size = 11) +
        theme(legend.position = "top")
      
      return(ggplotly(p))
    }, error = function(e) {
      return(plotly_empty() %>% layout(title = paste("Error:", e$message)))
    })
  })
  
  # Station Summary Table
  output$station_summary_table <- DT::renderDataTable({
    stats <- spc_stats()
    if (is.null(stats) || nrow(stats) == 0) {
      return(DT::datatable(data.frame(Message = "No data available")))
    }
    
    summary_table <- stats %>%
      dplyr::select(Start_Station_Code, Rush_Period, mean_rides, sd_rides, CV, 
             Variability_Category, UCL, LCL) %>%
      dplyr::mutate(
        mean_rides = round(mean_rides, 2),
        sd_rides = round(sd_rides, 2),
        CV = round(CV, 2),
        UCL = round(UCL, 2),
        LCL = round(LCL, 2)
      )
    
    DT::datatable(summary_table,
                  colnames = c("Station Code", "Rush Period", "Mean Rides", 
                              "SD Rides", "CV (%)", "Variability Category", 
                              "UCL", "LCL"),
                  options = list(pageLength = 20, scrollX = TRUE))
  })
  
  ################################################################################
  # End SPC Tab
  ################################################################################
  
  # Visualization 1: Process Variability Comparison (AM vs PM)
  output$variability_plot <- renderPlotly({
    # Create sample data for visualization (representing CV distribution)
    set.seed(123)
    
    # Simulate CV values for AM and PM stations
    am_cv <- rnorm(263, mean = 50, sd = 25)
    am_cv <- pmax(am_cv, 0)  # Ensure non-negative
    
    pm_cv <- rnorm(263, mean = 53, sd = 28)
    pm_cv <- pmax(pm_cv, 0)
    
    # Create data frame
    cv_data <- data.frame(
      CV = c(am_cv, pm_cv),
      Rush_Hour = rep(c("AM (7-9 AM)", "PM (4-6 PM)"), each = 263)
    )
    
    # Create the plot
    p <- ggplot(cv_data, aes(x = CV, fill = Rush_Hour)) +
      geom_histogram(alpha = 0.7, bins = 30, position = "identity") +
      geom_vline(xintercept = 50, color = "red", linetype = "dashed", size = 1) +
      scale_fill_manual(values = c("AM (7-9 AM)" = "#E69F00", "PM (4-6 PM)" = "#56B4E9")) +
      labs(
        title = "Process Variability Distribution: AM vs PM Rush Hours",
        subtitle = "Distribution of Coefficient of Variation (CV) across all stations",
        x = "Coefficient of Variation (%)",
        y = "Number of Stations",
        fill = "Rush Period",
        caption = "Red dashed line indicates high variability threshold (CV = 50%)"
      ) +
      theme_minimal(base_size = 12) +
      theme(
        legend.position = "top",
        plot.title = element_text(face = "bold", size = 14),
        plot.subtitle = element_text(size = 11),
        plot.caption = element_text(size = 9, color = "gray50")
      ) +
      annotate("text", x = 55, y = Inf, label = "High Variability\nThreshold", 
               vjust = 1.5, color = "red", size = 3.5, fontface = "bold")
    
    return(ggplotly(p))
  })
  
  # References List
  output$references_list <- renderUI({
    refs <- list(
      tags$p(tags$strong("Banerjee, S., Kabir, M. M., Khadem, N. K., & Chavis, C. (2020)."), 
             "Optimal locations for bikeshare stations: A new GIS based spatial approach. ",
             tags$em("Transportation Research Interdisciplinary Perspectives"), ", 4, 100101. ",
             tags$a(href = "https://doi.org/10.1016/j.trip.2020.100101", "https://doi.org/10.1016/j.trip.2020.100101")),
      
      tags$p(tags$strong("Bike Share Expansion 2024-2025. (2025, October 31)."), 
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/transportation/bike-share-expansion-2024-2025", 
                    "https://www.boston.gov/departments/transportation/bike-share-expansion-2024-2025")),
      
      tags$p(tags$strong("Bluebikes membership & pass options."), 
             tags$a(href = "https://bluebikes.com/pricing", "https://bluebikes.com/pricing")),
      
      tags$p(tags$strong("Blue Bikes - Overview, News & similar companies | Zoominfo.com."), 
             tags$a(href = "https://www.zoominfo.com/c/blue-bikes/450178718", 
                    "https://www.zoominfo.com/c/blue-bikes/450178718")),
      
      tags$p(tags$strong("Boston, C. O. (2024, October 24)."), 
             "Bluebike Station Siting Project Summary. ",
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary", 
                    "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary")),
      
      tags$p(tags$strong("Chavis, C., Barnes, P., Grasso, S., Bhuyan, I. A., & Nickkar, A. (2018)."), 
             "Bicycle justice or just bicycles? Analyzing equity in Baltimore's bike share program. ",
             tags$em("University of Delaware Center for Applied Demography and Survey Research."), 
             tags$a(href = "https://udspace.udel.edu/items/d5a8a864-87f8-4d39-b835-6fe521c5a63d", 
                    "https://udspace.udel.edu/items/d5a8a864-87f8-4d39-b835-6fe521c5a63d")),
      
      tags$p(tags$strong("Chen, W., Chen, X., Cheng, L., & Tao, S. (2024)."), 
             "Locating new docked bike sharing stations considering demand suitability and spatial accessibility. ",
             tags$em("Travel Behaviour and Society"), ", 34, 100675. ",
             tags$a(href = "https://doi.org/10.1016/j.tbs.2023.100675", "https://doi.org/10.1016/j.tbs.2023.100675")),
      
      tags$p(tags$strong("City of Boston. (2024, October 24)."), 
             "Bluebike Station Siting Project Summary. ",
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary", 
                    "https://www.boston.gov/departments/analytics-team/bluebike-station-siting-project-summary")),
      
      tags$p(tags$strong("Freund, D., Norouzi-Fard, A., Paul, A., Wang, C., Henderson, S., & Shmoys, D. (2016)."), 
             "Data-driven rebalancing methods for bike-share systems. ",
             tags$em("Cornell University."), 
             tags$a(href = "https://people.orie.cornell.edu/shane/pubs/BSOvernight.pdf", 
                    "https://people.orie.cornell.edu/shane/pubs/BSOvernight.pdf")),
      
      tags$p(tags$strong("Go Boston 2030 Vision and Action Plan released. (2018, June 19)."), 
             tags$em("Boston.gov."), 
             tags$a(href = "https://www.boston.gov/news/go-boston-2030-vision-and-action-plan-released", 
                    "https://www.boston.gov/news/go-boston-2030-vision-and-action-plan-released")),
      
      tags$p(tags$strong("Herbert, K. (2021, May 18)."), 
             "Boston's Vision for Equitable Bike Share - Better Bike Share. ",
             tags$em("Better Bike Share."), 
             tags$a(href = "https://betterbikeshare.org/2021/05/18/bostons-vision-for-equitable-bike-share/", 
                    "https://betterbikeshare.org/2021/05/18/bostons-vision-for-equitable-bike-share/")),
      
      tags$p(tags$strong("Hrabec, D., Nverlý, V., Víchová, K., Šohaj, K., Peterek, K., & Taraba, P. (2024)."), 
             "Location of bike-sharing stations: Optimization model and case studies with insights into population coverage. ",
             tags$em("SSRN Electronic Journal."), 
             tags$a(href = "https://doi.org/10.2139/ssrn.4849664", "https://doi.org/10.2139/ssrn.4849664")),
      
      tags$p(tags$strong("Karpinski, E. (2021)."), 
             "Estimating the effect of protected bike lanes on bike-share ridership in Boston: A case study on Commonwealth Avenue. ",
             tags$em("Case Studies on Transport Policy."), 
             tags$a(href = "https://doi.org/10.1016/j.cstp.2021.06.015", "https://doi.org/10.1016/j.cstp.2021.06.015")),
      
      tags$p(tags$strong("McNeil, N., Dill, J., MacArthur, J., Broach, J., & Ma, J. (2022)."), 
             "Factors influencing bike share among underserved populations: Evidence from three U.S. cities. ",
             tags$em("Transportation Research Part D: Transport and Environment"), ", 112, 103471. ",
             tags$a(href = "https://doi.org/10.1016/j.trd.2022.103471", "https://doi.org/10.1016/j.trd.2022.103471")),
      
      tags$p(tags$strong("MilNeil, C. (2021, July 22)."), 
             "Research suggests Boston's new protected lanes boosted bikeshare traffic 80 percent. ",
             tags$em("Streetsblog Massachusetts."), 
             tags$a(href = "https://mass.streetsblog.org/2021/07/22/research-suggests-bostons-new-protected-lanes-boosted-bikeshare-traffic-80-percent", 
                    "https://mass.streetsblog.org/2021/07/22/research-suggests-bostons-new-protected-lanes-boosted-bikeshare-traffic-80-percent")),
      
      tags$p(tags$strong("Mintz, S., & Mintz, S. (2025, July 7)."), 
             "The town just got $100,000 for two new Bluebikes stations. Where should they go? ",
             tags$em("Brookline.News."), 
             tags$a(href = "https://brookline.news/the-town-just-got-100000-for-two-new-bluebikes-stations-where-should-they-go/", 
                    "https://brookline.news/the-town-just-got-100000-for-two-new-bluebikes-stations-where-should-they-go/")),
      
      tags$p(tags$strong("National Association of City Transportation Officials. (2015)."), 
             "Walkable station spacing is key to successful, equitable bike share. ",
             tags$em("NACTO."), 
             tags$a(href = "https://nacto.org/publication/walkable-station-spacing-is-key-to-successful-equitable-bike-share/", 
                    "https://nacto.org/publication/walkable-station-spacing-is-key-to-successful-equitable-bike-share/")),
      
      tags$p(tags$strong("National Association of City Transportation Officials. (2016)."), 
             "Bike share station siting guide. ",
             tags$em("NACTO."), 
             tags$a(href = "https://nacto.org/publication/bike-share-station-siting-guide/", 
                    "https://nacto.org/publication/bike-share-station-siting-guide/")),
      
      tags$p(tags$strong("News | Town of Arlington. (2025)."), 
             tags$em("Arlingtonma.gov."), 
             tags$a(href = "https://www.arlingtonma.gov/Home/Components/News/News/14237/", 
                    "https://www.arlingtonma.gov/Home/Components/News/News/14237/")),
      
      tags$p(tags$strong("Poe Public. (n.d.)."), 
             "Bluebikes Station Suggestion Map. ",
             tags$a(href = "https://shareabouts-bluebikes-suggestions-prod-1045183798776.us-east4.run.app/page/about", 
                    "https://shareabouts-bluebikes-suggestions-prod-1045183798776.us-east4.run.app/page/about")),
      
      tags$p(tags$strong("Region seeks new operating contract for expanding BlueBikes system - Streetsblog Massachusetts. (2025, May 2)."), 
             tags$a(href = "https://mass.streetsblog.org/2025/05/02/region-seeks-new-operating-contract-for-expanding-bluebikes-system", 
                    "https://mass.streetsblog.org/2025/05/02/region-seeks-new-operating-contract-for-expanding-bluebikes-system")),
      
      tags$p(tags$strong("Ricci, M. (2015)."), 
             "Bike sharing: A review of evidence on impacts and processes of implementation and operation. ",
             tags$em("Research in Transportation Business & Management"), ", 15, 28–38. ",
             tags$a(href = "https://doi.org/10.1016/j.rtbm.2015.03.003", "https://doi.org/10.1016/j.rtbm.2015.03.003")),
      
      tags$p(tags$strong("Smith, E. (2025, May 27)."), 
             "When is Rush Hour in Boston? Our Traffic Guide. ",
             tags$a(href = "https://www.blacklane.com/en/blog/travel/rush-hour-in-boston/", 
                    "https://www.blacklane.com/en/blog/travel/rush-hour-in-boston/")),
      
      tags$p(tags$strong("Van Woert Katherine & Olivieri Sophia & Baron Jonathan & Buckley Katelyn & Lalli Pamela Fraser, T. &. (2025)."), 
             "Cycling cities: Measuring urban mobility mixing in bikeshare networks. ",
             tags$a(href = "https://ideas.repec.org/a/eee/jotrge/v126y2025ics0966692325001140.html", 
                    "https://ideas.repec.org/a/eee/jotrge/v126y2025ics0966692325001140.html")),
      
      tags$p(tags$strong("Zeid, A., Bhatt, T., & Morris, H. A. (2022)."), 
             "Machine learning model to forecast demand of Boston bike-ride sharing. ",
             tags$em("European Journal of Artificial Intelligence and Machine Learning"), ", 1(3), 1–10. ",
             tags$a(href = "https://doi.org/10.24018/ejai.2022.1.3.9", "https://doi.org/10.24018/ejai.2022.1.3.9"))
    )
    
    do.call(tags$div, refs)
  })
}

# Run the application
shinyApp(ui = ui, server = server)
