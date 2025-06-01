r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# Install libraries
#install.packages(c("shiny", "gtrendsR", "plotly", "leaflet", "sf", "httr", "jsonlite", "dplyr"))
#install.packages(c("rnaturalearth", "rnaturalearthdata"), dependencies = FALSE)
#install.packages("geodata")
#install.packages("shinybusy")
#install.packages("gemini.R") # https://github.com/jhk0530/gemini.R

# Load libraries
library(shiny)
library(gtrendsR)
library(plotly)
library(leaflet)
library(sf)
library(httr)
library(jsonlite)
library(dplyr)
library(geodata)
library(stringr)
library(stringi)
library(gemini.R)
library(rnaturalearth)
library(rnaturalearthdata)
# library(shinybusy)

# Load Gemini API key, https://ai.google.dev/gemini-api/docs/api-key
# setAPI()

# Load sample LLM analysis
all_analysis_text <- readLines("llm_output_5.txt", warn = FALSE) %>% paste(collapse = "\n")

# --- Split into five parts using regex ---
analysis_parts <- strsplit(all_analysis_text, "# Sample [0-9]+")[[1]]

# Note: strsplit will leave an empty string at the start if the file starts with "# Sample 1"
analysis_parts <- analysis_parts[nzchar(analysis_parts)]  # remove empty

# Assign to variables (you can also keep them as a list)
analysis_text_1 <- trimws(analysis_parts[1])
analysis_text_2 <- trimws(analysis_parts[2])
analysis_text_3 <- trimws(analysis_parts[3])
analysis_text_4 <- trimws(analysis_parts[4])
analysis_text_5 <- trimws(analysis_parts[5])


# Convert province to English
clean_location_names <- function(loc_vec) {
  loc_vec %>%
    # Remove " Province" suffix
    str_remove_all("\\s+Province$") %>%
    # Remove Vietnamese accents (normalize to ASCII)
    stri_trans_general(id = "Latin-ASCII") %>%
    # Trim whitespace
    str_trim() %>%
    # Replace specific names (add spaces, fix spelling)
    str_replace_all(c(
      "^Hanoi$" = "Ha Noi",
      "^HoChiMinh$" = "Ho Chi Minh",
      "^Haiphong$" = "Hai Phong"
    ))
}

# Load Vietnam map
vietnam_geo <- gadm(country = "VNM", level = 1, path = tempdir()) %>% 
  sf::st_as_sf()

# Load sample data
load("res1.RData")
load("res2.RData")
load("res3.RData")
load("res4.RData")
load("res5.RData")

# UI
ui <- fluidPage(
  tags$h3(
    icon("chart-line"), " Search Trend Analysis Dashboard",
    style = "color:#800000; font-family:monospace; background-color:#fff3cd; padding:10px; border-radius: 8px; box-shadow: 2px 2px 6px rgba(0,0,0,0.1);"
  ),
  
  fluidRow(
      column(3,
             div(style = "display: flex; align-items: center;",
                 div(style = "width: 25px; height: 25px; background-color: #1f77b4; margin-right: 8px;"),
                 textInput("kw1", tagList(icon("search"), "Keyword 1"), "")
             )
      ),
      column(3,
             div(style = "display: flex; align-items: center;",
                 div(style = "width: 25px; height: 25px; background-color: #ff7f0e; margin-right: 8px;"),
                 textInput("kw2", tagList(icon("search"), "Keyword 2"), "")
             )
      ),
      column(3,
             div(style = "display: flex; align-items: center;",
                 div(style = "width: 25px; height: 25px; background-color: #2ca02c; margin-right: 8px;"),
                 textInput("kw3", tagList(icon("search"), "Keyword 3"), "")
             )
      ),
    column(2, selectInput("data_source", tagList(icon("database"), "Samples"),
                          choices = c("Sample 1", "Sample 2", "Sample 3", "Sample 4", "Sample 5", "Online"),
                          selected = "Sample 1")),
    column(1, br(), actionButton("analyze", tagList(icon("play"), "Analyze"), class = "btn btn-primary"))
  ),
  
  wellPanel(
    h4(icon("chart-bar"), " Interest Over Time", style = "margin-left: 10px; color: #004085;"),
    fluidRow(
      column(3, plotOutput("barPlot", height = "200px")),
      column(9, plotlyOutput("lineChart", height = "200px"))
    ),
    
    br(),
    
    fluidRow(
      column(5,
             h4(icon("globe"), " Compared Breakdown by Province", style = "color: #004085;"),
             leafletOutput("map", height = "400px")
      ),
      
      column(7,
             h4(icon("stream"), " Keyword Trends", style = "color: #004085;"),
             plotlyOutput("streamgraphPlot", height = "400px")
      )
    )
  ),
  
  wellPanel(
    h4(icon("robot"), " LLM-generated Analysis", style = "margin-left: 10px; color: #004085;"),
    tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-size: 16px; background-color: #f8f9fa; padding: 10px; border: 1px solid #dee2e6; border-radius: 5px;",
             textOutput("llm_output"))
  ),
  tags$div(
    style = "margin-top: 20px; font-size: 12px; color: #6c757d;",
    tags$p("1) This dashboard utilizes an unofficial Google Trends API; the presented results may differ from those displayed on the official Google Trends website."),
    tags$p("2) The LLM-generated analyses are for informational purposes only and may contain inaccuracies. Users are advised to independently verify all insights and conclusions."),
    tags$p("3) Dashboard designed and developed by Doan Quang Hung and Phan Minh Tri.")
  )
)


# Server
server <- function(input, output, session) {
  # Define consistent colors for keywords dynamically
  keyword_colors <- reactive({
    res <- results()
    if (is.null(res)) return(NULL)
    kws <- unique(res$interest_over_time$keyword)
    # Example palette (recycled if needed)
    # palette <- RColorBrewer::brewer.pal(max(3, length(kws)), "Set2")
    palette <- c("#1f77b4", "#ff7f0e", "#2ca02c")  # blue, orange, green
    setNames(palette[1:length(kws)], kws)
  })
  
  # Reactive: get the correct dataset or gtrends result
  selected_data <- reactive({
    switch(input$data_source,
           "Sample 1" = res,
           "Sample 2" = res2,
           "Sample 3" = res3,
           "Sample 4" = res4,
           "Sample 5" = res5,
           "Online" = NULL)
  })
  
  # Observe selection and update textInputs with keywords (if in sample mode)
  observeEvent(input$data_source, {
    if (input$data_source != "Online") {
      sample_res <- selected_data()
      # Example: extract first 3 keywords from the dataset (adjust to your structure!)
      kw <- head(unique(sample_res$interest_over_time$keyword), 3)
      updateTextInput(session, "kw1", value = ifelse(length(kw) >= 1, kw[1], ""))
      updateTextInput(session, "kw2", value = ifelse(length(kw) >= 2, kw[2], ""))
      updateTextInput(session, "kw3", value = ifelse(length(kw) >= 3, kw[3], ""))
    } else {
      # Online mode: clear inputs
      updateTextInput(session, "kw1", value = "")
      updateTextInput(session, "kw2", value = "")
      updateTextInput(session, "kw3", value = "")
    }
  })
  
  # Event when Analyze button is clicked
  results <- eventReactive(input$analyze, {
    kw <- c(input$kw1, input$kw2, input$kw3)
    kw <- kw[kw != ""]
    if (length(kw) == 0) return(NULL)
    
    if (input$data_source == "Online") {
      gtrends(kw, gprop = "web", time = "today 3-m", geo = "VN")
    } else {
      selected_data()
    }
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)
    
    df <- res$interest_over_time
    df$hits <- as.numeric(as.character(df$hits))
    df <- df[!is.na(df$hits), ]
    
    # Calculate average hits per keyword
    avg_df <- aggregate(hits ~ keyword, data = df, FUN = mean, na.rm = TRUE)
    
    # Get colors (if you want to keep using your custom palette)
    cols <- keyword_colors()
    avg_df$color <- cols[avg_df$keyword]
    
    ggplot(avg_df, aes(x = keyword, y = hits, fill = keyword)) +
      geom_bar(stat = "identity") +
      geom_text(aes(label = round(hits, 1)), vjust = -0.5) +
      scale_fill_manual(values = cols) +
      labs(y = "Average Hits", x = NULL, fill = NULL) +
      expand_limits(y = max(avg_df$hits) * 1.1) +
      theme_minimal() +
      theme(
        axis.text.x = element_blank(),  # remove x-axis labels
        axis.ticks.x = element_blank(), # remove x-axis ticks
        legend.position = "none"        # remove legend
      )
  })
  
  
  
  # Line chart with consistent colors
  output$lineChart <- renderPlotly({
    res <- results()
    if (is.null(res)) return(NULL)
    df <- res$interest_over_time
    cols <- keyword_colors()
    
    plot_ly(df, x = ~date, y = ~hits, color = ~keyword,
            colors = cols,
            type = 'scatter', mode = 'lines')%>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Search Interest (Hits)"),
        legend = list(
          title = list(text = "Keywords"),
          x = 1,
          xanchor = "left",
          y = 1
        )
      )
  })
  
  output$streamgraphPlot <- renderPlotly({
    res <- results()
    if (is.null(res)) return(NULL)
    df <- res$interest_over_time
    
    df <- df %>%
      mutate(date = as.Date(date),
             hits = as.numeric(hits)) %>%
      filter(!is.na(hits))
    
    kws <- unique(df$keyword)
    cols <- keyword_colors()
    # cols <- c("#1f77b4", "#ff7f0e", "#2ca02c") # your color palette
    
    p <- plot_ly()
    
    for (i in seq_along(kws)) {
      kw <- kws[i]
      df_sub <- df %>% filter(keyword == kw)
      
      p <- add_trace(p,
                     x = ~date,
                     y = ~hits,
                     name = kw,
                     type = 'scatter',
                     mode = 'none',  # no markers or lines
                     stackgroup = 'one',
                     fillcolor = cols[(i - 1) %% length(cols) + 1],
                     data = df_sub)
    }
    
    p %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Search Interest (Hits)", zeroline = FALSE),
        legend = list(title = list(text = "Keywords"))
      )
  })

  output$map <- renderLeaflet({
    res <- results()
    if (is.null(res)) return(NULL)
    geo_data <- res$interest_by_region
    if (nrow(geo_data) == 0) return(NULL)
    
    geo_data$location_clean <- clean_location_names(geo_data$location)
    
    # Pivot to wide format: one row per location, each keyword as a column
    wide_geo <- geo_data %>%
      tidyr::pivot_wider(names_from = keyword, values_from = hits, values_fill = 0)
    
    # Calculate max-hit keyword and its value for coloring
    # Get list of keyword columns only
    keyword_cols <- setdiff(names(wide_geo), c("location", "location_clean", "geo", "gprop"))

    # Make sure hit_vals carries names
    wide_geo <- wide_geo %>%
      rowwise() %>%
      mutate(
        max_hits = max(c_across(all_of(keyword_cols)), na.rm = TRUE),
        max_keyword = {
          hit_vals <- c_across(all_of(keyword_cols))
          names(hit_vals) <- keyword_cols
          
          # Check if all values are NA or zero
          if (all(is.na(hit_vals)) || all(hit_vals == 0, na.rm = TRUE)) {
            max_name <- NA_character_  # or "None"
          } else {
            max_idx <- which.max(replace(hit_vals, is.na(hit_vals), -Inf))  # treat NA as -Inf
            max_name <- names(hit_vals)[max_idx]
          }
          
          # message("Row ", location_clean, " -> max_name: ", max_name, 
          #         " -> hit_vals: ", paste(hit_vals, collapse = ", "))
          max_name
        } %>% as.character()
      ) %>%
      ungroup()
    
    
    # Prepare Vietnam spatial data
    vietnam_geo_mod <- vietnam_geo %>%
      dplyr::rename(location_clean = VARNAME_1)
    
    # Merge with spatial data
    merged_data <- vietnam_geo_mod %>%
      left_join(wide_geo, by = "location_clean")
    
    # Create a color palette (one color per keyword)
    keyword_list <- unique(geo_data$keyword)
    colors_for_map <- keyword_colors()
    
    # Build color vector: assign color based on the max_keyword
    merged_data$fill_color <- colors_for_map[merged_data$max_keyword]
    
    # Create tooltip showing hits for all keywords
    merged_data$tooltip <- apply(merged_data, 1, function(row) {
      paste0(
        row["location_clean"], "<br>",
        paste(
          sapply(keyword_list, function(k) {
            paste0(k, ": ", round(as.numeric(row[[k]]), 2))
          }),
          collapse = "<br>"
        )
      )
    })
    
    # Load world polygons
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Create mask polygon: world minus Vietnam
    mask <- st_difference(st_union(world), st_union(vietnam_geo_mod))
    
    map <- leaflet(merged_data) %>%
      addTiles() %>%
      
      addPolygons(
        data = mask,
        fillColor = "gray",
        fillOpacity = 0.5,
        stroke = FALSE
      ) %>%
      
      setView(lng = 106, lat = 16, zoom = 5) %>%
      
      addPolygons(
        fillColor = ~fill_color,
        fillOpacity = 0.7,
        color = "white",
        weight = 1,
        dashArray = "3",
        opacity = 1,
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#666",
          dashArray = "",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(merged_data$tooltip, htmltools::HTML),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      )
    
    # Add legend
      map <- map %>%
        addLegend(
          colors = colors_for_map,
          labels = names(colors_for_map),
          opacity = 0.7,
          title = "Keywords",
          position = "bottomright"
        )
    
    map
    
  })
  
    
  output$llm_output <- renderText({
    res <- results()
    if (is.null(res)) return("Awaiting input...")
    
    if (input$data_source == "Online") {
      # --- Online mode: dynamically generate prompt ---
      df <- res$interest_over_time
      
      trend_summary <- paste(
        apply(df, 1, function(row) {
          paste0(
            "Date: ", row["date"], 
            ", Keyword: ", row["keyword"], 
            ", Hits: ", row["hits"]
          )
        }),
        collapse = "\n"
      )
      
      prompt <- paste0(
        "You are a data analyst. Based on the following Google Trends data over time:\n",
        trend_summary,
        "\nPlease write 3 to 5 sentences analyzing the trend of each keyword, and conclude any correlation or relationship between them."
      )
      
      gemini(prompt)  # 🚀 Call Gemini LLM here
      
    } else {
      # --- Sample mode: use preloaded analysis ---
      switch(input$data_source,
             "Sample 1" = analysis_text_1,
             "Sample 2" = analysis_text_2,
             "Sample 3" = analysis_text_3,
             "Sample 4" = analysis_text_4,
             "Sample 5" = analysis_text_5,
             "No analysis available.")
    }
  })
  
  remove_modal_spinner()
}

# Run app
options(shiny.autoreload = TRUE)
shinyApp(ui, server)