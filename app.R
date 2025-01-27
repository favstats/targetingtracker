library(shiny)
library(dplyr)
library(DT)
library(highcharter)
library(arrow)
library(metatargetr)

ui <- fluidPage(
  titlePanel("Targeting Tracker"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Data Loading Options"),
      dateInput("start_date", "Start Date", value = as.Date("2025-01-01")),
      dateInput("end_date", "End Date", value = Sys.Date()),
      textInput("country_filter", "Enter Country Code (e.g., DE)", value = "DE"),
      selectInput(
        "timeframe",
        "Select Timeframe",
        choices = c("Last 7 Days" = 7, "Last 30 Days" = 30, "Last 90 Days" = 90),
        selected = 7
      ),
      actionButton("load_data", "Load Data"),
      hr(),
      helpText("Specify the date range, country, and timeframe to load targeting data."),
    ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Data Table", DTOutput("filtered_table")),
        tabPanel("Graph", highchartOutput("entries_graph")),
        tabPanel("Summary", verbatimTextOutput("data_summary"))
      )
    )
  )
)

server <- function(input, output, session) {
  # Reactive storage for dataset
  dataset <- reactiveVal()
  
  # Load data when the user clicks "Load Data"
  observeEvent(input$load_data, {
    req(input$start_date, input$end_date, input$country_filter, input$timeframe)
    
    # Define the date range
    date_range <- seq(as.Date(input$start_date), as.Date(input$end_date), by = "day")
    total_dates <- length(date_range)
    
    # Fetch data with a progress bar
    withProgress(message = "Loading data", value = 0, {
      all_data <- bind_rows(lapply(seq_along(date_range), function(i) {
        date <- date_range[i]
        tryCatch({
          # Fetch data for the current date with user-specified timeframe
          data <- metatargetr::get_targeting_db(
            the_cntry = input$country_filter,
            tf = as.numeric(input$timeframe),  # Use selected timeframe
            ds = as.character(date)
          )
          # Increment progress
          incProgress(1 / total_dates, detail = paste("Processing date:", date))
          data
        }, error = function(e) {
          message(paste("Error fetching data for date:", date, "- Skipping."))
          NULL  # Return NULL on error
        })
      }))
      
      # Select and process columns
      selected_data <- all_data %>%
        select(page_id, page_name, target = value, detailed_type, num_ads, total_spend_pct, total_spend_formatted, ds) %>%
        mutate(
          total_spend_pct = round(total_spend_pct * 100, 2)  # Convert spend to percentage
        ) %>%
        filter(!is.na(detailed_type) & detailed_type != "")  # Drop rows with empty detailed_type
      
      # Store the combined dataset
      dataset(selected_data)
    })
  })
  
  # Render filtered data table
  output$filtered_table <- renderDT({
    req(dataset())
    datatable(
      dataset(),
      filter = "top",
      options = list(pageLength = 10),
      rownames = FALSE
    )
  })
  
  # Reactive filtered data from DT
  filtered_data <- reactive({
    req(dataset())
    # Capture filtered rows from the DT table
    dataset()[input$filtered_table_rows_all, ]
  })
  
  # Render interactive graph using Highcharter
  output$entries_graph <- renderHighchart({
    req(filtered_data())
    
    filtered_data() %>%
      group_by(ds) %>%
      summarise(num_ads = sum(num_ads, na.rm = TRUE), .groups = "drop") %>%
      hchart("line", hcaes(x = as.Date(ds), y = num_ads)) %>%
      hc_title(text = "Number of Ads Over Time (Filtered)") %>%
      hc_xAxis(title = list(text = "Date")) %>%
      hc_yAxis(title = list(text = "Number of Ads")) %>%
      hc_add_theme(hc_theme_flat())
  })
  
  # Render data summary
  output$data_summary <- renderPrint({
    req(filtered_data())
    
    filtered_data() %>%
      summarise(
        Total_Ads = sum(num_ads, na.rm = TRUE),
        Total_Spend_Pct = sum(total_spend_formatted, na.rm = TRUE),
        Unique_Pages = n_distinct(page_name)
      )
  })
}

shinyApp(ui, server)