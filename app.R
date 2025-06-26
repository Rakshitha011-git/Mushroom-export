library(shiny)
library(tidyverse)
library(shinythemes)
library(plotly)
library(DT)
library(shinydashboard)
library(RColorBrewer)

# --------------------------
# Data Preparation
# --------------------------

# Clean and prepare CSVs
clean_csv <- function(filename) {
  if(file.exists(filename)) {
    read.csv(filename, check.names = FALSE) %>%
      rename_with(str_trim) %>%
      mutate(Importers = str_trim(Importers))
  } else {
    # Create sample data if files don't exist
    data.frame(
      Importers = c("World", "France", "Germany", "USA", "Japan"),
      `Exported value in 2020` = c(1000, 200, 150, 100, 50),
      `Exported value in 2021` = c(1200, 250, 180, 120, 60),
      `Exported value in 2022` = c(1100, 230, 160, 110, 55),
      check.names = FALSE
    )
  }
}

# Convert to long format
to_long <- function(df, mushroom_type) {
  df %>%
    pivot_longer(cols = -Importers, names_to = "Year", values_to = "Value") %>%
    mutate(
      Year = as.integer(str_extract(Year, "\\d{4}")),
      Value = as.numeric(ifelse(is.na(Value) | Value == "", 0, Value)),
      MushroomType = mushroom_type
    ) %>%
    filter(!is.na(Year), !is.na(Value))
}

# Load and prepare data
canned <- clean_csv("canned 10 year.csv")
dried <- clean_csv("dried 10 year.csv") 
fresh <- clean_csv("fresh 10 year.csv")

# Convert to long format
canned_long <- to_long(canned, "Canned")
dried_long <- to_long(dried, "Dried")
fresh_long <- to_long(fresh, "Fresh")

# Combine all data
all_data <- bind_rows(canned_long, dried_long, fresh_long)

# Separate world data
world_data <- all_data %>% filter(Importers == "World")
country_data <- all_data %>% filter(Importers != "World", Value > 0)

# --------------------------
# UI
# --------------------------
ui <- dashboardPage(
  dashboardHeader(title = "🍄 India Mushroom Export Analytics Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("📊 Overview & Growth", tabName = "overview", icon = icon("chart-line")),
      menuItem("🌍 World Analysis", tabName = "world", icon = icon("globe")),
      menuItem("🏆 Top Importers", tabName = "importers", icon = icon("trophy")),
      menuItem("📈 Comparative Analysis", tabName = "comparison", icon = icon("balance-scale")),
      menuItem("📋 Data Explorer", tabName = "data", icon = icon("table"))
    ),
    
    hr(),
    h4("🎛️ Filters", style = "margin-left: 15px;"),
    
    checkboxGroupInput("mushroom_types", "Mushroom Types:",
                       choices = c("Canned", "Dried", "Fresh"),
                       selected = c("Canned", "Dried", "Fresh")),
    
    sliderInput("year_range_slider", "Year Range:",
                min = 2015, max = 2024, value = c(2015, 2024),
                step = 1, sep = ""),
    
    sliderInput("top_n_slider", "Top N Countries:",
                min = 5, max = 25, value = 10, step = 1),
    
    br(),
    downloadButton("download_filtered", "📥 Download Data", 
                   class = "btn-primary", style = "margin-left: 15px;")
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f4f4;
        }
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 10px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    tabItems(
      # Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(width = 12, title = "📈 Overall Export Growth Trends", status = "primary", solidHeader = TRUE,
              plotlyOutput("overall_growth_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 6, title = "🎯 Export Summary by Type", status = "info", solidHeader = TRUE,
              plotlyOutput("type_summary_plot", height = "350px")
          ),
          box(width = 6, title = "📅 Year-wise Performance", status = "success", solidHeader = TRUE,
              plotlyOutput("yearly_performance_plot", height = "350px")
          )
        )
      ),
      
      # World Analysis Tab
      tabItem(tabName = "world",
        fluidRow(
          box(width = 12, title = "🌍 World Export Trends", status = "primary", solidHeader = TRUE,
              plotlyOutput("world_trends_plot", height = "400px")
          )
        ),
        fluidRow(
          box(width = 6, title = "📊 World Export Distribution", status = "warning", solidHeader = TRUE,
              plotlyOutput("world_distribution_plot", height = "350px")
          ),
          box(width = 6, title = "📈 World Growth Analysis", status = "info", solidHeader = TRUE,
              plotlyOutput("world_growth_plot", height = "350px")
          )
        )
      ),
      
      # Top Importers Tab
      tabItem(tabName = "importers",
        fluidRow(
          box(width = 12, title = "🏆 Top Importing Countries Analysis", status = "primary", solidHeader = TRUE,
              plotlyOutput("top_importers_plot", height = "600px")
          )
        ),
        fluidRow(
          box(width = 4, title = "🎯 Market Leaders", status = "success", solidHeader = TRUE,
              plotlyOutput("market_leaders_plot", height = "350px")
          ),
          box(width = 4, title = "📊 Market Share Pie", status = "info", solidHeader = TRUE,
              plotlyOutput("market_share_pie", height = "350px")
          ),
          box(width = 4, title = "📈 Growth Champions", status = "warning", solidHeader = TRUE,
              DT::dataTableOutput("growth_champions_table")
          )
        )
      ),
      
      # Comparative Analysis Tab
      tabItem(tabName = "comparison",
        fluidRow(
          box(width = 12, title = "⚖️ Mushroom Type Comparison", status = "primary", solidHeader = TRUE,
              plotlyOutput("comparison_plot", height = "450px")
          )
        ),
        fluidRow(
          box(width = 6, title = "🔄 Cross-Category Analysis", status = "warning", solidHeader = TRUE,
              plotlyOutput("cross_category_plot", height = "350px")
          ),
          box(width = 6, title = "📈 Growth Rate Comparison", status = "success", solidHeader = TRUE,
              plotlyOutput("growth_comparison_plot", height = "350px")
          )
        )
      ),
      
      # Data Explorer Tab
      tabItem(tabName = "data",
        fluidRow(
          box(width = 12, title = "🔍 Interactive Data Explorer", status = "primary", solidHeader = TRUE,
              DT::dataTableOutput("data_explorer_table")
          )
        )
      )
    )
  )
)

# --------------------------
# Server
# --------------------------
server <- function(input, output, session) {
  
  # Reactive data filtering
  filtered_data <- reactive({
    req(input$mushroom_types)
    data <- country_data %>%
      filter(MushroomType %in% input$mushroom_types,
             Year >= input$year_range_slider[1],
             Year <= input$year_range_slider[2])
    return(data)
  })
  
  filtered_world_data <- reactive({
    req(input$mushroom_types)
    world_data %>%
      filter(MushroomType %in% input$mushroom_types,
             Year >= input$year_range_slider[1],
             Year <= input$year_range_slider[2])
  })
  
  # Get top countries
  top_countries <- reactive({
    req(nrow(filtered_data()) > 0)
    filtered_data() %>%
      group_by(Importers) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalValue)) %>%
      slice_head(n = input$top_n_slider) %>%
      pull(Importers)
  })
  
  # Overall Growth Plot
  output$overall_growth_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      group_by(Year, MushroomType) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = Year, y = TotalValue, color = MushroomType)) +
      geom_line(size = 1.2) +
      geom_point(size = 3) +
      labs(title = "Overall Export Growth Trends",
           x = "Year", y = "Export Value (Tonnes)",
           color = "Mushroom Type") +
      theme_minimal(base_size = 12) +
      scale_color_brewer(type = "qual", palette = "Set1") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # Type Summary Plot
  output$type_summary_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      group_by(MushroomType) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = reorder(MushroomType, TotalValue), y = TotalValue, fill = MushroomType)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      labs(title = "Total Exports by Mushroom Type",
           x = "Mushroom Type", y = "Total Export Value (Tonnes)") +
      theme_minimal() +
      scale_fill_brewer(type = "qual", palette = "Set2") +
      theme(legend.position = "none")
    
    ggplotly(p)
  })
  
  # Yearly Performance Plot
  output$yearly_performance_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      group_by(Year) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = Year, y = TotalValue)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      labs(title = "Year-wise Export Performance",
           x = "Year", y = "Total Export Value (Tonnes)") +
      theme_minimal() +
      scale_x_continuous(breaks = seq(2015, 2024, 1))
    
    ggplotly(p)
  })
  
  # World Trends Plot
  output$world_trends_plot <- renderPlotly({
    req(nrow(filtered_world_data()) > 0)
    data <- filtered_world_data()
    
    p <- ggplot(data, aes(x = Year, y = Value, color = MushroomType)) +
      geom_line(size = 1.5) +
      geom_point(size = 4) +
      labs(title = "World Export Trends by Mushroom Type",
           x = "Year", y = "World Export Value (Tonnes)",
           color = "Mushroom Type") +
      theme_minimal(base_size = 12) +
      scale_color_brewer(type = "qual", palette = "Dark2") +
      theme(legend.position = "bottom")
    
    ggplotly(p) %>%
      layout(hovermode = "x unified")
  })
  
  # World Distribution Plot
  output$world_distribution_plot <- renderPlotly({
    req(nrow(filtered_world_data()) > 0)
    data <- filtered_world_data() %>%
      group_by(MushroomType) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      mutate(Percentage = round(TotalValue / sum(TotalValue) * 100, 1))
    
    # Create pie chart using plotly directly
    plot_ly(data, 
            labels = ~MushroomType, 
            values = ~TotalValue,
            type = 'pie',
            textinfo = 'label+percent',
            textposition = 'inside',
            hovertemplate = '<b>%{label}</b><br>Value: %{value}<br>Percentage: %{percent}<extra></extra>',
            marker = list(colors = RColorBrewer::brewer.pal(length(unique(data$MushroomType)), "Pastel1"))) %>%
      layout(title = list(text = "World Export Distribution", font = list(size = 16)),
             showlegend = TRUE,
             legend = list(orientation = "v", x = 1, y = 0.5))
  })
  
  # World Growth Plot
  output$world_growth_plot <- renderPlotly({
    req(nrow(filtered_world_data()) > 0)
    data <- filtered_world_data() %>%
      group_by(MushroomType) %>%
      arrange(Year) %>%
      mutate(GrowthRate = (Value - lag(Value)) / lag(Value) * 100) %>%
      filter(!is.na(GrowthRate), !is.infinite(GrowthRate))
    
    if(nrow(data) > 0) {
      p <- ggplot(data, aes(x = Year, y = GrowthRate, fill = MushroomType)) +
        geom_bar(stat = "identity", position = "dodge") +
        labs(title = "World Export Growth Rate (%)",
             x = "Year", y = "Growth Rate (%)",
             fill = "Mushroom Type") +
        theme_minimal() +
        scale_fill_brewer(type = "qual", palette = "Set3") +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red")
      
      ggplotly(p)
    } else {
      plotly_empty() %>%
        layout(title = "No growth data available for selected filters")
    }
  })
  
  # Top Importers Plot
  output$top_importers_plot <- renderPlotly({
    req(length(top_countries()) > 0)
    
    # Get top 5 countries for cleaner visualization
    top_5_countries <- filtered_data() %>%
      group_by(Importers) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalValue)) %>%
      slice_head(n = 5) %>%
      pull(Importers)
    
    # Line chart for top 5 countries
    line_data <- filtered_data() %>%
      filter(Importers %in% top_5_countries) %>%
      group_by(Importers, Year) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
    
    # Bar chart for all selected countries (total values)
    bar_data <- filtered_data() %>%
      filter(Importers %in% top_countries()) %>%
      group_by(Importers) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalValue)) %>%
      slice_head(n = 15)  # Show top 15 for bar chart
    
    # Create subplot with line chart and bar chart
    p1 <- plot_ly(line_data, x = ~Year, y = ~TotalValue, color = ~Importers, 
                  type = 'scatter', mode = 'lines+markers',
                  line = list(width = 3), marker = list(size = 6)) %>%
      layout(title = "Top 5 Countries - Trend Analysis",
             xaxis = list(title = "Year"),
             yaxis = list(title = "Import Value (Tonnes)"),
             showlegend = TRUE)
    
    p2 <- plot_ly(bar_data, x = ~TotalValue, y = ~reorder(Importers, TotalValue), 
                  type = 'bar', orientation = 'h',
                  marker = list(color = ~TotalValue, colorscale = 'Viridis', showscale = FALSE)) %>%
      layout(title = "Top 15 Countries - Total Imports",
             xaxis = list(title = "Total Import Value (Tonnes)"),
             yaxis = list(title = "Country"),
             showlegend = FALSE)
    
    # Combine plots
    subplot(p1, p2, nrows = 2, heights = c(0.6, 0.4), 
            subplot_titles = c("Trend Analysis (Top 5)", "Total Imports Ranking (Top 15)")) %>%
      layout(title = list(text = "Top Importing Countries Analysis", 
                         font = list(size = 18)),
             height = 600)
  })
  
  # Market Leaders Plot
  output$market_leaders_plot <- renderPlotly({
    req(length(top_countries()) > 0)
    data <- filtered_data() %>%
      group_by(Importers) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalValue)) %>%
      slice_head(n = 10)
    
    plot_ly(data, x = ~TotalValue, y = ~reorder(Importers, TotalValue), 
            type = 'bar', orientation = 'h',
            marker = list(color = ~TotalValue, 
                         colorscale = list(c(0, '#3498DB'), c(1, '#E74C3C')), 
                         showscale = FALSE),
            text = ~paste(TotalValue, "tonnes"), textposition = 'outside') %>%
      layout(title = "Top 10 Market Leaders",
             xaxis = list(title = "Total Import Value (Tonnes)"),
             yaxis = list(title = ""),
             margin = list(l = 120))
  })
  
  # Market Share Pie Chart
  output$market_share_pie <- renderPlotly({
    req(length(top_countries()) > 0)
    data <- filtered_data() %>%
      group_by(Importers) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(desc(TotalValue)) %>%
      slice_head(n = 8) %>%
      mutate(Percentage = round(TotalValue / sum(TotalValue) * 100, 1))
    
    plot_ly(data, 
            labels = ~Importers, 
            values = ~TotalValue,
            type = 'pie',
            textinfo = 'label+percent',
            hovertemplate = '<b>%{label}</b><br>Value: %{value} tonnes<br>Share: %{percent}<extra></extra>',
            marker = list(colors = RColorBrewer::brewer.pal(8, "Set3"))) %>%
      layout(title = list(text = "Market Share (Top 8)", font = list(size = 14)),
             showlegend = FALSE)
  })
  
  # Growth Champions Table
  output$growth_champions_table <- DT::renderDataTable({
    req(length(top_countries()) > 0)
    data <- filtered_data() %>%
      group_by(Importers) %>%
      arrange(Year) %>%
      summarise(
        FirstYear = first(Value[Value > 0]),
        LastYear = last(Value[Value > 0]),
        GrowthRate = round(((LastYear - FirstYear) / FirstYear) * 100, 1),
        TotalValue = sum(Value, na.rm = TRUE),
        .groups = "drop"
      ) %>%
      filter(TotalValue > 50, !is.na(GrowthRate), !is.infinite(GrowthRate)) %>%
      arrange(desc(GrowthRate)) %>%
      slice_head(n = 10) %>%
      select(Country = Importers, `Growth %` = GrowthRate, `Total Value` = TotalValue)
    
    if(nrow(data) > 0) {
      DT::datatable(data, 
                    options = list(pageLength = 10, dom = 't', 
                                  columnDefs = list(list(className = 'dt-center', targets = 1:2))), 
                    rownames = FALSE) %>%
        formatStyle('Growth %', 
                    background = styleColorBar(range(data$`Growth %`), '#2ECC71'),
                    backgroundSize = '80% 90%',
                    backgroundRepeat = 'no-repeat',
                    backgroundPosition = 'center')
    } else {
      DT::datatable(data.frame(Message = "No data available"), options = list(dom = 't'), rownames = FALSE)
    }
  })
  
  # Comparison Plot
  output$comparison_plot <- renderPlotly({
    req(length(top_countries()) > 0)
    data <- filtered_data() %>%
      filter(Importers %in% top_countries()) %>%
      group_by(Importers, MushroomType) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop")
    
    p <- ggplot(data, aes(x = Importers, y = TotalValue, fill = MushroomType)) +
      geom_bar(stat = "identity", position = "dodge") +
      labs(title = "Mushroom Type Comparison by Country",
           x = "Country", y = "Total Value (Tonnes)",
           fill = "Mushroom Type") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      scale_fill_brewer(type = "qual", palette = "Set1")
    
    ggplotly(p)
  })
  
  # Cross Category Plot
  output$cross_category_plot <- renderPlotly({
    req(length(input$mushroom_types) >= 2)
    data <- filtered_data() %>%
      group_by(MushroomType, Year) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      pivot_wider(names_from = MushroomType, values_from = TotalValue, values_fill = 0)
    
    # Create correlation plot if we have multiple types
    types <- input$mushroom_types[1:2]
    if(all(types %in% names(data)) && nrow(data) > 1) {
      p <- ggplot(data, aes_string(x = types[1], y = types[2])) +
        geom_point(size = 3, alpha = 0.7) +
        geom_smooth(method = "lm", se = FALSE) +
        labs(title = paste("Correlation:", types[1], "vs", types[2]),
             x = paste(types[1], "Exports"), y = paste(types[2], "Exports")) +
        theme_minimal()
      
      ggplotly(p)
    } else {
      plotly_empty() %>%
        layout(title = "Select at least 2 mushroom types for correlation analysis")
    }
  })
  
  # Growth Comparison Plot
  output$growth_comparison_plot <- renderPlotly({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      group_by(MushroomType, Year) %>%
      summarise(TotalValue = sum(Value, na.rm = TRUE), .groups = "drop") %>%
      arrange(MushroomType, Year) %>%
      group_by(MushroomType) %>%
      mutate(GrowthRate = (TotalValue - lag(TotalValue)) / lag(TotalValue) * 100) %>%
      filter(!is.na(GrowthRate), !is.infinite(GrowthRate))
    
    if(nrow(data) > 0) {
      p <- ggplot(data, aes(x = Year, y = GrowthRate, color = MushroomType)) +
        geom_line(size = 1) +
        geom_point(size = 2) +
        labs(title = "Growth Rate Comparison",
             x = "Year", y = "Growth Rate (%)",
             color = "Mushroom Type") +
        theme_minimal() +
        geom_hline(yintercept = 0, linetype = "dashed", color = "red") +
        scale_color_brewer(type = "qual", palette = "Dark2")
      
      ggplotly(p)
    } else {
      plotly_empty() %>%
        layout(title = "No growth data available for selected filters")
    }
  })
  
  # Data Explorer Table
  output$data_explorer_table <- DT::renderDataTable({
    req(nrow(filtered_data()) > 0)
    data <- filtered_data() %>%
      arrange(desc(Value))
    
    DT::datatable(
      data,
      options = list(
        pageLength = 15,
        scrollX = TRUE,
        dom = 'Bfrtip'
      ),
      rownames = FALSE,
      filter = 'top'
    )
  })
  
  # Download Handler
  output$download_filtered <- downloadHandler(
    filename = function() {
      paste0("mushroom_export_data_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(filtered_data(), file, row.names = FALSE)
    }
  )
}

# Run the app
shinyApp(ui = ui, server = server)