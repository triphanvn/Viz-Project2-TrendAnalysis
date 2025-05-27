r = getOption("repos")
r["CRAN"] = "http://cran.us.r-project.org"
options(repos = r)

# Install libraries
#install.packages(c("shiny", "gtrendsR", "plotly", "leaflet", "sf", "httr", "jsonlite", "dplyr"))
#install.packages(c("rnaturalearth", "rnaturalearthdata"), dependencies = FALSE)
#install.packages("geodata")
#install.packages("openai")

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
      "^HoChiMinh$" = "Ho Chi Minh"  # example if needed
      # Add more replacements here if needed
    ))
}

# Load Vietnam map
vietnam_geo <- gadm(country = "VNM", level = 1, path = tempdir()) %>% 
  sf::st_as_sf()

# UI
ui <- fluidPage(
  tags$h3("Search Trend Analysis Dashboard", style = "color:#800000; font-family:monospace; background-color:#fff3cd; padding:10px;"),
  fluidRow(
    column(2, textInput("kw1", "Keyword 1", "")),
    column(2, textInput("kw2", "Keyword 2", "")),
    column(2, textInput("kw3", "Keyword 3", "")),
    column(2, br(), actionButton("analyze", "Analyze"))
  ),
  fluidRow(
    column(3, plotOutput("barPlot", height = "200px")),
    column(9, plotlyOutput("lineChart", height = "200px"))
  ),
  fluidRow(
    column(6, leafletOutput("map", height = "300px")),
    column(6, plotlyOutput("scatterPlot", height = "300px"))
  ),
  h4("LLM-generated Analysis"),
  verbatimTextOutput("llm_output")
)

# Server
server <- function(input, output, session) {
  
  # Define consistent colors for keywords dynamically
  keyword_colors <- reactive({
    res <- results()
    if (is.null(res)) return(NULL)
    kws <- unique(res$interest_over_time$keyword)
    # Choose any palette or fixed colors
    palette <- c("#1f77b4", "#ff7f0e", "#2ca02c")  # blue, orange, green
    # Map keyword to color (length might vary, so recycle if needed)
    setNames(palette[1:length(kws)], kws)
  })
  
  results <- eventReactive(input$analyze, {
    kw <- c(input$kw1, input$kw2, input$kw3)
    kw <- kw[kw != ""]
    if (length(kw) == 0) return(NULL)
    res
    # gtrends(kw, gprop = "web", time = "today 3-m", geo = "VN")
  })
  
  # Bar plot
  output$barPlot <- renderPlot({
    res <- results()
    if (is.null(res)) return(NULL)

    df <- res$interest_over_time
    df$hits <- as.numeric(as.character(df$hits))
    df <- df[!is.na(df$hits), ]

    avg_hits <- tapply(df$hits, df$keyword, mean)

    cols <- keyword_colors()
    bar_cols <- cols[names(avg_hits)]

    barplot(avg_hits,
            main = "Average Hits",
            col = bar_cols,
            horiz = FALSE,
            ylab = "Average Hits",
            xlab = NULL,
            las = 2,
            names.arg = rep("", length(avg_hits))
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
            type = 'scatter', mode = 'lines')
  })
  
  # Scatter plot with consistent colors (optional, if you have one)
  output$scatterPlot <- renderPlotly({
    res <- results()
    if (is.null(res)) return(NULL)
    df <- res$interest_over_time
    cols <- keyword_colors()
    
    plot_ly(df, x = ~date, y = ~hits, color = ~keyword,
            colors = cols,
            type = "scatter", mode = "markers")
  })
  
  output$map <- renderLeaflet({
    res <- results()
    if (is.null(res)) return(NULL)
    geo_data <- res$interest_by_region
    if (nrow(geo_data) == 0) return(NULL)
    
    cleaned_locations <- clean_location_names(res$interest_by_region$location)
    geo_data$location_clean <- clean_location_names(geo_data$location)
    
    # Rename spatial data column for easier join & labeling
    vietnam_geo_mod <- vietnam_geo %>% 
      dplyr::rename(location_clean = VARNAME_1)
    
    # Join Google Trends data with Vietnam spatial data by "location"
    merged_data <- vietnam_geo_mod %>% 
      left_join(geo_data, by = "location_clean")
    
    # Create color palette based on hits
    pal <- colorNumeric("YlOrRd", domain = merged_data$hits, na.color = "transparent")

    # Load world polygons (medium scale) as sf
    world <- ne_countries(scale = "medium", returnclass = "sf")
    
    # Create mask polygon: world minus Vietnam
    mask <- st_difference(st_union(world), st_union(vietnam_geo_mod))
    
    leaflet(merged_data) %>%
      addTiles() %>%
      
      # Add gray mask polygon for everything except Vietnam
      addPolygons(
        data = mask,
        fillColor = "gray",
        fillOpacity = 0.5,
        stroke = FALSE
      ) %>%
      
      setView(lng = 106, lat = 16, zoom = 4) %>%
      
      addPolygons(
        fillColor = ~pal(hits),
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
        label = ~paste0(location_clean, ": ", round(hits, 2)),
        labelOptions = labelOptions(
          style = list("font-weight" = "normal", padding = "3px 8px"),
          textsize = "15px",
          direction = "auto"
        )
      ) %>%
      addLegend(
        pal = pal,
        values = ~hits,
        opacity = 0.7,
        title = "Hits",
        position = "bottomright"
      )
  })
  

  # output$llm_output <- renderText({
  #   res <- results()
  #   if (is.null(res)) return("Awaiting input...")
  #   df <- res$interest_over_time
  #   text_input <- paste(capture.output(head(df, 20)), collapse = "\n")
  #   get_llm_analysis(text_input)
  # })
}

# Run app
options(shiny.autoreload = TRUE)
shinyApp(ui, server)