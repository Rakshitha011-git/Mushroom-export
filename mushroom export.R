library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(DT)

# --------------------------
# UI
# --------------------------
ui <- fluidPage(
  theme = shinytheme("flatly"),
  titlePanel("Export Data of Mushrooms from India"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Mushroom Category"),
      tabsetPanel(
        id = "dataset_tabs",
        tabPanel("Canned", value = "canned"),
        tabPanel("Dried", value = "dried"),
        tabPanel("Fresh", value = "fresh")
      ),
      br(),
      selectInput("year_range", "Select Year Range:",
                  choices = c("All Years", "First 5 Years", "Last 5 Years")),
      selectInput("chart_type", "Chart Type:",
                  choices = c("Line Chart", "Bar Chart")),
      uiOutput("year_selector"),
      hr(),
      h4("Customize Axes & Filter"),
      selectInput("x_axis", "X-axis:", choices = c("Year", "Importers")),
      sliderInput("top_n", "Filter: Top N Importers by Value",
                  min = 3, max = 20, value = 10, step = 1),
      br(),
      downloadButton("download_data", "Download Filtered Data")
    ),
    
    mainPanel(
      tabsetPanel(
        id = "main_tabset",
        
        tabPanel("Summary & Table",
                 plotlyOutput("summary_plot", height = "450px"),
                 br(),
                 h4("Filtered Export Data Table"),
                 DT::dataTableOutput("summary_table")
        ),
        
        tabPanel("Year-wise Chart",
                 plotlyOutput("yearwise_plot", height = "600px")
        )
      ),
      br(),
      uiOutput("world_total_ui")
    )
  )
)

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
  
  # Clean and prepare CSVs
  clean_csv <- function(filename) {
    read.csv(filename, check.names = FALSE) %>%
      rename_with(str_trim) %>%
      mutate(Importers = str_trim(Importers))
  }
  
  canned <- clean_csv("canned 10 year.csv")
  dried  <- clean_csv("dried 10 year.csv")
  fresh  <- clean_csv("fresh 10 year.csv")
  
  to_long <- function(df) {
    df %>%
      pivot_longer(cols = -Importers, names_to = "Year", values_to = "Value") %>%
      mutate(
        Year = as.integer(str_trim(Year)),
        Value = as.numeric(Value)
      ) %>%
      drop_na()
  }
  
  canned_long <- to_long(canned)
  dried_long  <- to_long(dried)
  fresh_long  <- to_long(fresh)
  
  selected_data <- reactive({
    switch(input$dataset_tabs,
           "canned" = canned_long,
           "dried" = dried_long,
           "fresh" = fresh_long)
  })
  
  output$year_selector <- renderUI({
    years <- sort(unique(selected_data()$Year))
    selectInput("year_choice", "Select Year for Year-wise Comparison:",
                choices = years, selected = years[1])
  })
  
  filter_years <- function(df) {
    years <- sort(unique(df$Year))
    if (input$year_range == "First 5 Years") {
      head(years, 5)
    } else if (input$year_range == "Last 5 Years") {
      tail(years, 5)
    } else {
      years
    }
  }
  
  get_top_importers <- function(df, top_n = 10) {
    df %>%
      filter(Importers != "World") %>%
      group_by(Importers) %>%
      summarise(Total = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(Total)) %>%
      slice_head(n = top_n) %>%
      pull(Importers)
  }
  
  render_summary_plot <- function(df) {
    years_range <- filter_years(df)
    df <- df %>% filter(Year %in% years_range)
    
    top_importers <- get_top_importers(df, input$top_n)
    df <- df %>% filter(Importers %in% top_importers)
    
    x <- sym(input$x_axis)
    y <- sym("Value")
    
    title <- paste("Export Comparison -", str_to_title(input$dataset_tabs), "Mushrooms")
    
    if (input$chart_type == "Line Chart") {
      p <- ggplot(df, aes(x = !!x, y = !!y, group = Importers, color = Importers,
                          text = paste("Importer:", Importers, "<br>Year:", Year, "<br>Value:", Value))) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = title, x = input$x_axis, y = "Export Quantity (Tonnes)") +
        theme_minimal(base_size = 14)
    } else {
      p <- ggplot(df %>% group_by(!!x) %>% summarise(Total = sum(!!y, na.rm = TRUE), .groups = "drop"),
                  aes(x = reorder(!!x, Total), y = Total, fill = !!x,
                      text = paste(input$x_axis, ":", !!x, "<br>Total:", Total))) +
        geom_bar(stat = "identity") +
        coord_flip() +
        labs(title = title, x = input$x_axis, y = "Total Export Value") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "none")
    }
    
    ggplotly(p, tooltip = "text")
  }
  
  render_yearwise_plot <- function(df) {
    df <- df %>% filter(Year == input$year_choice, Importers != "World")
    
    top_importers <- get_top_importers(df, input$top_n)
    df <- df %>% filter(Importers %in% top_importers)
    
    p <- ggplot(df, aes(x = reorder(Importers, Value), y = Value, fill = Importers,
                        text = paste("Importer:", Importers, "<br>Value:", Value))) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = paste("Year-wise Export -", input$year_choice),
           x = "Importers", y = "Quantity (Tonnes)") +
      theme_minimal(base_size = 14) +
      theme(legend.position = "none")
    
    ggplotly(p, tooltip = "text")
  }
  
  # ----------- Plot Outputs --------------
  output$summary_plot <- renderPlotly({
    render_summary_plot(selected_data())
  })
  
  output$yearwise_plot <- renderPlotly({
    render_yearwise_plot(selected_data())
  })
  
  output$world_total_ui <- renderUI({
    df <- selected_data()
    total <- df %>%
      filter(Year == input$year_choice, Importers == "World") %>%
      summarise(Total = sum(Value, na.rm = TRUE)) %>%
      pull(Total)
    
    if (!is.na(total)) {
      strong(paste("🌍 World Total Export in", input$year_choice, ":", format(total, big.mark = ",")))
    } else {
      ""
    }
  })
  
  # ----------- Table Output --------------
  filtered_table_data <- reactive({
    df <- selected_data()
    top_importers <- get_top_importers(df, input$top_n)
    years_range <- filter_years(df)
    
    df %>%
      filter(Importers %in% top_importers, Year %in% years_range) %>%
      arrange(desc(Value))
  })
  
  output$summary_table <- DT::renderDataTable({
    DT::datatable(
      filtered_table_data(),
      options = list(pageLength = 10, autoWidth = TRUE),
      rownames = FALSE
    )
  })
  
  # ----------- Download --------------
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("mushroom_export_", input$dataset_tabs, "_data.csv")
    },
    content = function(file) {
      write.csv(filtered_table_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)