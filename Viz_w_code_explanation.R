# Set CRAN repository for package installation
r = getOption("repos")  # Retrieves current repository settings
r["CRAN"] = "http://cran.us.r-project.org"  # Sets CRAN mirror to a US-based server for package downloads
options(repos = r)  # Updates global repository settings

# Install libraries (commented out to avoid repeated installation)
#install.packages(c("shiny", "gtrendsR", "plotly", "leaflet", "sf", "httr", "jsonlite", "dplyr"))  # Core packages for app functionality
#install.packages(c("rnaturalearth", "rnaturalearthdata"), dependencies = FALSE)  # Geospatial data packages
#install.packages("geodata")  # For downloading geospatial data
#install.packages("shinybusy")  # For adding loading indicators (not used in this code)
#install.packages("gemini.R")  # Custom package for Gemini API integration (https://github.com/jhk0530/gemini.R)

# Load libraries with purpose explanations
library(shiny)  # Purpose: Creates interactive web applications; used here to build the dashboard UI and server logic
library(gtrendsR)  # Purpose: Interfaces with Google Trends API to fetch search trend data; chosen for its direct access to Google Trends data
library(plotly)  # Purpose: Generates interactive plots (e.g., line charts, streamgraphs); selected for its rich interactivity and web compatibility
library(leaflet)  # Purpose: Creates interactive maps; chosen for its robust geospatial visualization capabilities and compatibility with Shiny
library(sf)  # Purpose: Handles geospatial data (e.g., shapefiles); selected for its modern, efficient handling of spatial objects
library(httr)  # Purpose: Makes HTTP requests; used here likely for Gemini API calls (though not directly in code)
library(jsonlite)  # Purpose: Parses JSON data; likely used for processing API responses
library(dplyr)  # Purpose: Provides data manipulation tools (e.g., filtering, grouping); chosen for its intuitive syntax and performance
library(geodata)  # Purpose: Downloads geospatial data (e.g., country boundaries); selected for easy access to Vietnam’s administrative boundaries
library(stringr)  # Purpose: Handles string operations (e.g., cleaning location names); chosen for its robust string manipulation functions
library(stringi)  # Purpose: Advanced string processing (e.g., accent removal); selected for its comprehensive Unicode support
library(gemini.R)  # Purpose: Custom package for Gemini LLM API integration; chosen to generate AI-driven analysis
library(rnaturalearth)  # Purpose: Provides access to natural earth geospatial data; used for world map polygons
library(rnaturalearthdata)  # Purpose: Supplies additional geospatial datasets; paired with rnaturalearth for complete data access
# library(shinybusy)  # Purpose: Adds loading indicators to Shiny apps; commented out, likely unused in final version

# Load Gemini API key (commented out, assumed to be set elsewhere)
# setAPI()  # Function from gemini.R to configure Gemini API key; not shown but critical for LLM integration

# Load pre-generated LLM analysis from file
all_analysis_text <- readLines("llm_output_5.txt", warn = FALSE) %>% paste(collapse = "\n")  
# readLines: Reads text file line by line; warn = FALSE suppresses warnings for incomplete final lines
# paste: Combines lines into a single string with newline separators
# Purpose: Loads precomputed LLM analysis for sample datasets to avoid real-time API calls during testing

# Split LLM analysis into parts based on sample identifiers
analysis_parts <- strsplit(all_analysis_text, "# Sample [0-9]+")[[1]]  
# strsplit: Splits string at "# Sample X" pattern; returns a list, taking first element
# Purpose: Separates combined LLM output into individual sample analyses
# Note: Assumes file format starts with "# Sample 1", may produce empty first element

analysis_parts <- analysis_parts[nzchar(analysis_parts)]  # nzchar: Checks for non-empty strings; removes empty elements
# Purpose: Cleans up empty strings from strsplit (e.g., if file starts with delimiter)

# Assign individual analyses to variables for easy access
analysis_text_1 <- trimws(analysis_parts[1])  # trimws: Removes leading/trailing whitespace
analysis_text_2 <- trimws(analysis_parts[2])  # Purpose: Ensures clean text for display
analysis_text_3 <- trimws(analysis_parts[3])
analysis_text_4 <- trimws(analysis_parts[4])
analysis_text_5 <- trimws(analysis_parts[5])
# Note: Hard-coded for 5 samples; could be made dynamic with a loop if needed

# Function to clean Vietnamese location names
clean_location_names <- function(loc_vec) {
  loc_vec %>%
    # Remove " Province" suffix
    str_remove_all("\\s+Province$") %>%  # str_remove_all: Removes all matches of pattern; here, removes " Province"
    # Remove Vietnamese accents (normalize to ASCII)
    stri_trans_general(id = "Latin-ASCII") %>%  # stri_trans_general: Converts text to ASCII, removing diacritics
    # Trim whitespace
    str_trim() %>%  # str_trim: Removes leading/trailing whitespace
    # Replace specific names for consistency
    str_replace_all(c(  # str_replace_all: Replaces matched patterns with specified strings
      "^Hanoi$" = "Ha Noi",  # Standardizes city names
      "^HoChiMinh$" = "Ho Chi Minh",
      "^Haiphong$" = "Hai Phong"
    ))
}
# Purpose: Standardizes location names for matching with geospatial data; handles Vietnamese diacritics and naming conventions
# Why chosen: Ensures consistency between Google Trends data and geospatial dataset; stri_trans_general is robust for Unicode handling

# Load Vietnam map data
vietnam_geo <- gadm(country = "VNM", level = 1, path = tempdir()) %>% 
  sf::st_as_sf()
# gadm: Downloads administrative boundaries (level 1 = provinces) for Vietnam from geodata package
# st_as_sf: Converts to simple features (sf) object for geospatial operations
# Purpose: Provides spatial data for mapping search trends by province
# Why chosen: gadm provides reliable, up-to-date administrative boundaries; sf is standard for geospatial analysis in R

# Load sample datasets
load("res1.RData")  # load: Loads RData file containing precomputed Google Trends results
load("res2.RData")  # Purpose: Uses preloaded data for testing/demonstration to avoid repeated API calls
load("res3.RData")
load("res4.RData")
load("res5.RData")
# Note: Assumes res1, res2, etc., contain gtrendsR output with interest_over_time and interest_by_region data

# UI Definition
ui <- fluidPage(  # fluidPage: Creates a responsive Shiny layout
  tags$h3(  # tags$h3: HTML header for dashboard title
    icon("chart-line"), " Search Trend Analysis Dashboard",  # icon: Adds FontAwesome icon
    style = "color:#800000; font-family:monospace; background-color:#fff3cd; padding:10px; border-radius: 8px; box-shadow: 2px 2px 6px rgba(0,0,0,0.1);"
  ),
  
  fluidRow(  # fluidRow: Organizes UI elements in a responsive row
    column(3,  # column: Defines a column (width = 3/12 of page)
      div(style = "display: flex; align-items: center;",  # div: Container for aligned input and color indicator
        div(style = "width: 25px; height: 25px; background-color: #1f77b4; margin-right: 8px;"),  # Color box for keyword 1
        textInput("kw1", tagList(icon("search"), "Keyword 1"), "")  # textInput: Input field for keyword 1
      )
    ),
    column(3,  # Similar structure for keyword 2
      div(style = "display: flex; align-items: center;",
        div(style = "width: 25px; height: 25px; background-color: #ff7f0e; margin-right: 8px;"),
        textInput("kw2", tagList(icon("search"), "Keyword 2"), "")
      )
    ),
    column(3,  # Similar structure for keyword 3
      div(style = "display: flex; align-items: center;",
        div(style = "width: 25px; height: 25px; background-color: #2ca02c; margin-right: 8px;"),
        textInput("kw3", tagList(icon("search"), "Keyword 3"), "")
      )
    ),
    column(2, selectInput("data_source", tagList(icon("database"), "Samples"),  # selectInput: Dropdown for selecting data source
      choices = c("Sample 1", "Sample 2", "Sample 3", "Sample 4", "Sample 5", "Online"),
      selected = "Sample 1")),
    column(1, br(), actionButton("analyze", tagList(icon("play"), "Analyze"), class = "btn btn-primary"))  # actionButton: Triggers analysis
  ),
  
  wellPanel(  # wellPanel: Creates a styled container for plots
    h4(icon("chart-bar"), " Interest Over Time", style = "margin-left: 10px; color: #004085;"),
    fluidRow(
      column(3, plotOutput("barPlot", height = "200px")),  # plotOutput: Renders static ggplot bar plot
      column(9, plotlyOutput("lineChart", height = "200px"))  # plotlyOutput: Renders interactive Plotly line chart
    ),
    
    br(),  # br: Adds line break for spacing
    
    fluidRow(
      column(5,
        h4(icon("globe"), " Compared Breakdown by Province", style = "color: #004085;"),
        leafletOutput("map", height = "400px")  # leafletOutput: Renders interactive Leaflet map
      ),
      
      column(7,
        h4(icon("stream"), " Keyword Trends", style = "color: #004085;"),
        plotlyOutput("streamgraphPlot", height = "400px")  # plotlyOutput: Renders interactive Plotly streamgraph
      )
    )
  ),
  
  wellPanel(
    h4(icon("robot"), " LLM-generated Analysis", style = "margin-left: 10px; color: #004085;"),
    tags$pre(style = "white-space: pre-wrap; word-wrap: break-word; font-size: 16px; background-color: #f8f9fa; padding: 10px; border: 1px solid #dee2e6; border-radius: 5px;",
      textOutput("llm_output"))  # textOutput: Displays LLM-generated text analysis
  ),
  tags$div(  # tags$div: Container for footer notes
    style = "margin-top: 20px; font-size: 12px; color: #6c757d;",
    tags$p("1) This dashboard utilizes an unofficial Google Trends API; the presented results may differ from those displayed on the official Google Trends website."),
    tags$p("2) The LLM-generated analyses are for informational purposes only and may contain inaccuracies. Users are advised to independently verify all insights and conclusions."),
    tags$p("3) Dashboard designed and developed by Doan Quang Hung and Phan Minh Tri.")
  )
)
# Purpose: Defines a user-friendly Shiny UI with inputs for keywords, a data source selector, and outputs for plots and text
# Why chosen: Shiny’s fluidPage and related functions provide a flexible, responsive layout; wellPanel organizes content cleanly

# Server Logic
server <- function(input, output, session) {
  # Define consistent colors for keywords
  keyword_colors <- reactive({  # reactive: Creates a reactive expression that updates with input changes
    res <- results()
    if (is.null(res)) return(NULL)
    kws <- unique(res$interest_over_time$keyword)  # unique: Extracts unique keywords from data
    palette <- c("#1f77b4", "#ff7f0e", "#2ca02c")  # Fixed color palette (blue, orange, green)
    setNames(palette[1:length(kws)], kws)  # setNames: Assigns colors to keywords
  })
  # Purpose: Ensures consistent colors across plots for each keyword
  # Why chosen: Fixed palette ensures visual consistency; reactive updates colors dynamically based on data

  # Select dataset based on input
  selected_data <- reactive({
    switch(input$data_source,  # switch: Selects dataset based on input$data_source
      "Sample 1" = res,
      "Sample 2" = res2,
      "Sample 3" = res3,
      "Sample 4" = res4,
      "Sample 5" = res5,
      "Online" = NULL)
  })
  # Purpose: Retrieves appropriate dataset (preloaded or NULL for online mode)
  # Why chosen: switch is concise for mapping input choices to datasets

  # Update text inputs when data source changes
  observeEvent(input$data_source, {  # observeEvent: Runs code when input$data_source changes
    if (input$data_source != "Online") {
      sample_res <- selected_data()
      kw <- head(unique(sample_res$interest_over_time$keyword), 3)  # head: Takes first 3 unique keywords
      updateTextInput(session, "kw1", value = ifelse(length(kw) >= 1, kw[1], ""))  # updateTextInput: Updates input field
      updateTextInput(session, "kw2", value = ifelse(length(kw) >= 2, kw[2], ""))
      updateTextInput(session, "kw3", value = ifelse(length(kw) >= 3, kw[3], ""))
    } else {
      updateTextInput(session, "kw1", value = "")
      updateTextInput(session, "kw2", value = "")
      updateTextInput(session, "kw3", value = "")
    }
  })
  # Purpose: Populates keyword inputs with sample data keywords or clears them for online mode
  # Why chosen: observeEvent ensures UI updates dynamically; ifelse handles cases with fewer than 3 keywords

  # Fetch data when Analyze button is clicked
  results <- eventReactive(input$analyze, {  # eventReactive: Runs when input$analyze is clicked
    kw <- c(input$kw1, input$kw2, input$kw3)
    kw <- kw[kw != ""]  # Filters out empty inputs
    if (length(kw) == 0) return(NULL)
    
    if (input$data_source == "Online") {
      gtrends(kw, gprop = "web", time = "today 3-m", geo = "VN")  # gtrends: Fetches Google Trends data for keywords
    } else {
      selected_data()
    }
  })
  # Purpose: Retrieves Google Trends data (online mode) or uses preloaded sample data
  # Why chosen: eventReactive ensures data is fetched only on button click; gtrends is tailored for Google Trends API

  # Bar plot (static)
  output$barPlot <- renderPlot({  # renderPlot: Renders ggplot for Shiny output
    res <- results()
    if (is.null(res)) return(NULL)
    
    df <- res$interest_over_time
    df$hits <- as.numeric(as.character(df$hits))  # as.numeric/as.character: Converts hits to numeric, handling factors
    df <- df[!is.na(df$hits), ]  # Filters out NA values
    
    avg_df <- aggregate(hits ~ keyword, data = df, FUN = mean, na.rm = TRUE)  # aggregate: Computes mean hits per keyword
    cols <- keyword_colors()
    avg_df$color <- cols[avg_df$keyword]
    
    ggplot(avg_df, aes(x = keyword, y = hits, fill = keyword)) +  # ggplot: Creates bar plot
      geom_bar(stat = "identity") +  # geom_bar: Draws bars with heights from data
      geom_text(aes(label = round(hits, 1)), vjust = -0.5) +  # geom_text: Adds labels above bars
      scale_fill_manual(values = cols) +  # scale_fill_manual: Sets custom colors
      labs(y = "Average Hits", x = NULL, fill = NULL) +
      expand_limits(y = max(avg_df$hits) * 1.1) +  # expand_limits: Adds padding to y-axis
      theme_minimal() +  # theme_minimal: Clean plot theme
      theme(
        axis.text.x = element_blank(),  # Removes x-axis labels
        axis.ticks.x = element_blank(),
        legend.position = "none"  # Hides legend (colors shown in UI inputs)
      )
  })
  # Purpose: Displays average search interest per keyword as a bar plot
  # Why chosen: ggplot provides high-quality static plots; geom_bar is ideal for comparing averages

  # Line chart (interactive)
  output$lineChart <- renderPlotly({  # renderPlotly: Renders interactive Plotly plot
    res <- results()
    if (is.null(res)) return(NULL)
    df <- res$interest_over_time
    cols <- keyword_colors()
    
    plot_ly(df, x = ~date, y = ~hits, color = ~keyword,  # plot_ly: Creates Plotly line chart
      colors = cols, type = 'scatter', mode = 'lines') %>%
      layout(
        xaxis = list(title = "Date"),
        yaxis = list(title = "Search Interest (Hits)"),
        legend = list(title = list(text = "Keywords"), x = 1, xanchor = "left", y = 1)
      )
  })
  # Purpose: Shows search interest trends over time for each keyword
  # Why chosen: Plotly’s interactivity (e.g., hover, zoom) enhances user experience; scatter/lines is suitable for time series

  # Streamgraph plot
  output$streamgraphPlot <- renderPlotly({
    res <- results()
    if (is.null(res)) return(NULL)
    df <- res$interest_over_time
    
    df <- df %>%
      mutate(date = as.Date(date), hits = as.numeric(hits)) %>%  # mutate: Converts date to Date, hits to numeric
      filter(!is.na(hits))  # filter: Removes NA hits
    
    kws <- unique(df$keyword)
    cols <- keyword_colors()
    
    p <- plot_ly()
    for (i in seq_along(kws)) {  # Loop to add each keyword’s trace
      kw <- kws[i]
      df_sub <- df %>% filter(keyword == kw)
      
      p <- add_trace(p, x = ~date, y = ~hits, name = kw, type = 'scatter', mode = 'none',  # add_trace: Adds stacked area
        stackgroup = 'one', fillcolor = cols[(i - 1) %% length(cols) + 1], data = df_sub)
    }
    
    p %>% layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "Search Interest (Hits)", zeroline = FALSE),
      legend = list(title = list(text = "Keywords"))
    )
  })
  # Purpose: Visualizes keyword trends as a stacked streamgraph to show relative contributions
  # Why chosen: Streamgraphs highlight proportional changes over time; Plotly ensures interactivity

  # Interactive map
  output$map <- renderLeaflet({
    res <- results()
    if (is.null(res)) return(NULL)
    geo_data <- res$interest_by_region
    if (nrow(geo_data) == 0) return(NULL)
    
    geo_data$location_clean <- clean_location_names(geo_data$location)  # Clean location names for matching
    
    wide_geo <- geo_data %>%
      tidyr::pivot_wider(names_from = keyword, values_from = hits, values_fill = 0)  # pivot_wider: Converts to wide format
    # Purpose: Creates columns for each keyword’s hits per location
    
    # Calculate max-hit keyword and its value for coloring
    # Get list of keyword columns only
    keyword_cols <- setdiff(names(wide_geo), c("location", "location_clean", "geo", "gprop"))
    
    wide_geo <- wide_geo %>%
      rowwise() %>%  # rowwise: Applies operations per row
      mutate(
        max_hits = max(c_across(all_of(keyword_cols)), na.rm = TRUE),  # c_across: Selects keyword columns
        max_keyword = {  # Determines keyword with maximum hits
          hit_vals <- c_across(all_of(keyword_cols))
          names(hit_vals) <- keyword_cols
          if (all(is.na(hit_vals)) || all(hit_vals == 0, na.rm = TRUE)) {
            max_name <- NA_character_
          } else {
            max_idx <- which.max(replace(hit_vals, is.na(hit_vals), -Inf))  # which.max: Finds index of max value
            max_name <- names(hit_vals)[max_idx]
          }
          max_name
        } %>% as.character()
      ) %>% ungroup()  # ungroup: Returns to standard data frame operations
    
    vietnam_geo_mod <- vietnam_geo %>% dplyr::rename(location_clean = VARNAME_1)  # rename: Aligns column name
    
    merged_data <- vietnam_geo_mod %>% left_join(wide_geo, by = "location_clean")  # left_join: Merges spatial and trend data
    
    keyword_list <- unique(geo_data$keyword)
    colors_for_map <- keyword_colors()
    merged_data$fill_color <- colors_for_map[merged_data$max_keyword]  # Assigns color based on dominant keyword
    
    merged_data$tooltip <- apply(merged_data, 1, function(row) {  # apply: Creates HTML tooltips
      paste0(row["location_clean"], "<br>", paste(
        sapply(keyword_list, function(k) paste0(k, ": ", round(as.numeric(row[[k]]), 2))),
        collapse = "<br>"
      ))
    })
    
    world <- ne_countries(scale = "medium", returnclass = "sf")  # ne_countries: Loads world map data
    mask <- st_difference(st_union(world), st_union(vietnam_geo_mod))  # st_difference/st_union: Creates mask for non-Vietnam areas
    
    map <- leaflet(merged_data) %>%  # leaflet: Initializes map
      addTiles() %>%  # addTiles: Adds base map tiles
      addPolygons(data = mask, fillColor = "gray", fillOpacity = 0.5, stroke = FALSE) %>%  # Masks non-Vietnam areas
      setView(lng = 106, lat = 16, zoom = 5) %>%  # setView: Centers map on Vietnam
      addPolygons(
        fillColor = ~fill_color, fillOpacity = 0.7, color = "white", weight = 1, dashArray = "3", opacity = 1,
        highlightOptions = highlightOptions(weight = 3, color = "#666", dashArray = "", fillOpacity = 0.9, bringToFront = TRUE),
        label = lapply(merged_data$tooltip, htmltools::HTML),  # Adds interactive tooltips
        labelOptions = labelOptions(style = list("font-weight" = "normal", padding = "3px 8px"), textsize = "15px", direction = "auto")
      ) %>%
      addLegend(colors = colors_for_map, labels = names(colors_for_map), opacity = 0.7, title = "Keywords", position = "bottomright")  # addLegend: Adds color legend
    map
  })
  # Purpose: Displays a map of Vietnam with provinces colored by the dominant keyword
  # Why chosen: Leaflet provides interactive maps; sf and geospatial functions ensure accurate province mapping

  # LLM analysis output
  output$llm_output <- renderText({
    res <- results()
    if (is.null(res)) return("Awaiting input...")
    
    if (input$data_source == "Online") {
      df <- res$interest_over_time
      trend_summary <- paste(
        apply(df, 1, function(row) paste0("Date: ", row["date"], ", Keyword: ", row["keyword"], ", Hits: ", row["hits"])),
        collapse = "\n"
      )
      prompt <- paste0(
        "You are a senior Data Analyst with strong experience in interpreting complex datasets. For each dataset, table, or chart I provide, analyze the key patterns, trends, or anomalies.",
        trend_summary,
        "Your task: Write 3 to 5 clear, concise sentences. Describe the trend of each keyword (e.g. rising, falling, seasonal). Conclude any correlation or relationship between the keywords. Use plain English that a non-technical reader (like a marketer or product manager) can easily understand"
      )
      gemini(prompt)  # gemini: Calls Gemini LLM to generate analysis (assumed from gemini.R)
    } else {
      switch(input$data_source,
        "Sample 1" = analysis_text_1,
        "Sample 2" = analysis_text_2,
        "Sample 3" = analysis_text_3,
        "Sample 4" = analysis_text_4,
        "Sample 5" = analysis_text_5,
        "No analysis available.")
    }
  })
  # Purpose: Displays LLM-generated analysis (preloaded for samples, real-time for online mode)
  # Why chosen: switch for sample data is efficient; gemini integrates AI analysis for online mode
}

# Run the Shiny app
options(shiny.autoreload = TRUE)  # Enables auto-reloading during development
shinyApp(ui, server)  # shinyApp: Launches the Shiny application
# Purpose: Combines UI and server logic to create the interactive dashboard