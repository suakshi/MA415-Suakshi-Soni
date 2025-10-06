# Sri Lanka Island Profile Shiny App

library(shiny)
library(shinythemes)
library(leaflet)
library(ggplot2)
library(plotly)
library(DT)
library(tidyr)
library(readr)
library(shinydashboard)
library(shinyWidgets)

# Load data
datasets <- list(
  pop     = read_csv("neiss/population.csv"),
  gdp     = read_csv("neiss/gdp.csv"),
  tour    = read_csv("neiss/tourism.csv"),
  ethn   = read_csv("neiss/ethnicity.csv"),
  relig   = read_csv("neiss/religion.csv"),
  pyramid_pop = read_csv("neiss/pop_pyramid.csv"),
  exports = data.frame(
    Commodity          = c("Tea","Textiles","Rubber","Coconut","Spices"),
    Value_USD_Millions = c(1400,4500,800,300,220),
    stringsAsFactors = FALSE
  )
)

# City definitions
cities <- list(
  Colombo = list(coords=c(6.9271,79.8612), image="colombo.jpg",
                 stats=data.frame(
                   Metric=c("Population (2025 est)","Elevation","Founded"),
                   Value=c("601,000","1 m ASL","Late 16th century"),stringsAsFactors=FALSE
                 ),
                 desc="Sri Lanka's commercial capital, blending colonial architecture and modern development."),
  Kandy = list(coords=c(7.2906,80.6337), image="kandy.jpeg",
               stats=data.frame(
                 Metric=c("Population (2025 est)","Elevation","Founded"),
                 Value=c("125,000","500 m ASL","14th century"),stringsAsFactors=FALSE
               ),
               desc="Cultural heartland famed for the Temple of the Tooth relic and scenic lake."),
  Galle = list(coords=c(6.0330,80.2160), image="galle.jpg",
               stats=data.frame(
                 Metric=c("Population (2025 est)","Elevation","Founded"),
                 Value=c("101,000","0–5 m ASL","1588"),stringsAsFactors=FALSE
               ),
               desc="Historic seaside city centered around the Galle Fort UNESCO site.")
)

# Biodiversity hotspots
hotspots <- data.frame(
  name = c('Sinharaja Forest','Horton Plains','Knuckles Range','Yala NP','Udawalawe NP'),
  lat  = c(6.4108,6.8025,7.4847,6.3617,6.4400),
  lng  = c(80.4446,80.8226,80.7677,81.5436,81.0045),
  stringsAsFactors = FALSE
)

# UI definition
ui <- fluidPage(
  theme = shinytheme("flatly"),
  tags$head(
    tags$style(HTML("
      .well { 
        background-color: #f8f9fa;
        border-radius: 10px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .section-title {
        border-bottom: 2px solid #2C3E50;
        padding-bottom: 10px;
        margin-bottom: 20px;
      }
      .nav-tabs {
        margin-bottom: 20px;
        font-weight: 500;
      }
      .city-card {
        border: 1px solid #ddd;
        border-radius: 8px;
        padding: 15px;
        margin-top: 20px;
        background-color: white;
      }
      .data-source {
        font-style: italic;
        color: #7b8a8b;
        margin-top: 20px;
        padding-top: 10px;
        border-top: 1px solid #ecf0f1;
      }
      .footer {
        margin-top: 30px;
        padding: 15px;
        text-align: center;
        background: #ecf0f1;
        color: #2C3E50;
        border-radius: 5px;
      }
      .leaflet-container {
        border-radius: 8px;
        box-shadow: 0 2px 5px rgba(0,0,0,0.1);
      }
      .stat-box {
        background-color: #f8f9fa;
        border-radius: 5px;
        padding: 10px;
        margin-bottom: 10px;
      }
    "))
  ),
  
  # Header with logo and title
  fluidRow(
    column(12, 
           div(
             style = "background-color: #2C3E50; padding: 10px 0; margin-bottom: 20px; border-radius: 5px;",
             div(
               style = "display: flex; align-items: center; padding: 0 20px;",
               img(src = "flag_sri_lanka.png", height = 40, alt = "Flag"),
               h2(style = "color: white; margin-left: 15px;", "Sri Lanka Explorer")
             )
           )
    )
  ),
  
  navbarPage(
    title = "", # Empty title since we have a custom header above
    id = "navTab",
    
    # Geography
    tabPanel("Geography",
             fluidRow(
               column(3,
                      wellPanel(
                        h3("Map Settings", class = "section-title"),
                        selectInput("city", "Map Focus", choices = c("Sri Lanka (All)", names(cities))),
                        hr(),
                        h4("Island Overview"),
                        img(src = "sri-lanka.jpg", width = "100%", alt = "Sri Lanka Map", 
                            style = "border-radius: 8px; margin-bottom: 15px;"),
                        div(style = "max-height: 300px; overflow-y: auto;",
                            p("Sri Lanka is a small island country located in the Indian Ocean, just south of India. It is known for its diverse landscapes, ranging from golden beaches to lush forests and towering mountains. The country has a rich cultural history, with influences from ancient kingdoms, colonial periods, and its deep Buddhist traditions. Sri Lanka's warm climate and natural beauty attract tourists from all over the world, making it a popular travel destination."),
                            p("The population of Sri Lanka is made up of several ethnic groups, with the Sinhalese being the largest group, followed by Tamils and other smaller communities. The country’s official languages are Sinhala and Tamil, while English is widely spoken, especially in urban areas. Sri Lanka’s history is marked by both ancient achievements and more recent challenges, including a long civil conflict that ended in 2009, but the country has made significant progress since then."),
                            p("Sri Lanka’s economy is primarily based on agriculture, with tea, rubber, and coconut being key exports. In addition to agriculture, the country is developing its industries and services sectors, including tourism, textiles, and information technology. Despite facing challenges such as natural disasters and political instability, Sri Lanka continues to show resilience and is working towards building a more stable and prosperous future for its people.")
                        )
                      )
               ),
               column(9,
                      h3("Interactive Map", class = "section-title"),
                      leafletOutput("mapCity", height = "500px"),
                      conditionalPanel(
                        condition = "input.city != 'Sri Lanka (All)'",
                        div(class = "city-card",
                            fluidRow(
                              column(4, 
                                     uiOutput("cityImage_ui"),
                                     tags$p(class = "text-center", textOutput("cityName"))
                              ),
                              column(8, 
                                     h4("About", class = "section-title"),
                                     p(textOutput("cityDesc")),
                                     h4("Key Statistics"),
                                     DTOutput("cityStats")
                              )
                            )
                        )
                      )
               )
             )
    ),
    
    # Demographics
    tabPanel("Demographics",
             fluidRow(
               column(12, h2("Population Demographics", class = "section-title"))
             ),
             fluidRow(
               column(6,
                      div(class = "stat-box",
                          h3("Ethnic Composition"), 
                          plotlyOutput("ethPie", height = "350px")
                      ),
                      div(class = "stat-box",
                          h3("Religious Composition"), 
                          plotlyOutput("relBar", height = "350px")
                      )
               ),
               column(6,  # Half width of the page (6 out of 12 columns)
                      div(class = "stat-box",
                          h3("Population Pyramid (2024)"),
                          plotOutput("pyramid_plot", height="630px"),  # Plot takes full width
                          br(),  # Add a line break for space between plot and text
                          h4("Key Demographic Trends"),
                          tags$ul(
                            tags$li("Shows an aging population with a broader middle age range"),
                            tags$li("Birth rates have declined over the past decade"),
                            tags$li("Life expectancy has increased to 77.5 years"),
                            tags$li("Working-age population (15-64) constitutes 67% of total")
                          )
                      )
               )
             ),
             div(class = "data-source", 
                 "Sources: 2012 Census, UN Population Division, Department of Census and Statistics Sri Lanka")
    ),
    
    # Economy
    tabPanel("Economy",
             fluidRow(
               column(12, h2("Economic Indicators", class = "section-title"))
             ),
             fluidRow(
               column(6,
                      div(class = "stat-box",
                          h3("GDP Over Time"), 
                          plotlyOutput("gdpPlot", height = "300px")
                      )
               ),
               column(6,
                      div(class = "stat-box",
                          h3("Tourism Recovery"), 
                          plotlyOutput("tourismBar", height = "300px")
                      )
               )
             ),
             br(),
             fluidRow(
               column(6,
                      div(class = "stat-box",
                          h3("Export Commodities"),
                          DTOutput("exportTable")
                      )
               ),
               column(6,
                      div(class = "stat-box",
                          h3("Export Value Chart"), 
                          plotlyOutput("exportBar", height = "300px")
                      )
               )
             ),
             div(class = "data-source", "Data from Central Bank of Sri Lanka and Export Development Board.")
    ),
    
    # Environment
    tabPanel("Environment",
             fluidRow(
               column(12, h2("Biodiversity & Conservation", class = "section-title"))
             ),
             fluidRow(
               column(7,
                      div(class = "stat-box",
                          h3("Biodiversity Hotspots"),
                          leafletOutput("mapHotspots", height = "450px")
                      )
               ),
               column(5,
                      div(class = "stat-box",
                          h3(style = "color: #2C3E50; border-bottom: 2px solid #3498DB; padding-bottom: 8px; margin-bottom: 15px;", 
                             "Conservation Areas"),
                          DTOutput("hotspotTable"),
                          br(),
                          h4(style = "color: #2C3E50; margin-top: 15px;", tags$i(class = "fa fa-leaf"), " Ecological Significance"),
                          div(style = "background-color: #f8f9fa; padding: 10px; border-left: 4px solid #2ecc71; border-radius: 4px;",
                              p("Sri Lanka is one of the world's 36 biodiversity hotspots, with remarkably high species diversity and endemism despite its relatively small size."),
                              p(style = "margin-bottom: 0;", "The island's geographic isolation and varied climate have resulted in unique ecosystems ranging from wet zone rainforests to dry zone savannas.")
                          )
                      )
               )
             ),
             br(),
             fluidRow(
               column(12,
                      div(class = "stat-box",
                          h3(style = "color: #2C3E50; border-bottom: 2px solid #3498DB; padding-bottom: 8px; margin-bottom: 15px;", 
                             "Key Wildlife Species"),
                          fluidRow(
                            column(6, 
                                   dataTableOutput("speciesTable")
                            ),
                            column(6,
                                   div(class = "conservation-status-box",
                                       style = "height: 100%; padding: 15px; border-left: 1px solid #ddd;",
                                       h4(style = "color: #2C3E50; border-bottom: 2px solid #3498DB; padding-bottom: 8px; margin-bottom: 15px;", 
                                          "Conservation Status"),
                                       div(class = "status-item",
                                           h5(tags$i(class = "fa fa-paw"), " Wildlife Protection"),
                                           p("Sri Lanka has designated 26% of its land area under some form of wildlife protection, including national parks, sanctuaries, and forest reserves.")
                                       ),
                                       div(class = "status-item", style = "margin-top: 15px;",
                                           h5(tags$i(class = "fa fa-exclamation-triangle"), " Key Threats"),
                                           tags$ul(
                                             tags$li("Habitat fragmentation due to development"),
                                             tags$li("Human-wildlife conflict in border communities"),
                                             tags$li("Climate change impacts on fragile ecosystems"),
                                             tags$li("Illegal poaching and wildlife trafficking")
                                           )
                                       )
                                   )
                            )
                          )
                      )
               )
             ),
             div(class = "data-source", "Data from Department of Wildlife Conservation, IUCN Red List, and Forest Department")
    ),
    
    # Politics
    tabPanel("Politics",
             fluidRow(
               column(12, h2("Political Structure", class = "section-title"))
             ),
             fluidRow(
               column(6,
                      div(class = "stat-box",
                          h3("Administrative Divisions"),
                          img(src = "administrative_map.png", width = "60%", alt = "Divisions"),
                          p("The island is divided into 9 provinces with varied demographics and economies.")
                      )
               ),
               column(6,
                      div(class = "stat-box",
                          h3("Provinces & Capitals"), 
                          DTOutput("polTable")
                      ),
                      br(),
                      div(class = "stat-box",
                          h3("Current Leadership"),
                          tags$ul(
                            tags$li(tags$b("President:"), " Anura Kumara Dissanayake (elected 2024)"),
                            tags$li(tags$b("Prime Minister:"), " Harini Amarasuriya"),
                            tags$li(tags$b("Form of Government:"), " Unitary semi-presidential republic"),
                            tags$li(tags$b("Legislature:"), " Parliament of Sri Lanka (225 members)")
                          )
                      )
               )
             ),
             div(class = "data-source", "Information current as of May 2025. Sources: Parliament of Sri Lanka, Election Commission.")
    )
  ),
)

# Server logic
server <- function(input, output, session) {
  pal <- colorFactor(c("#2980B9", "#E74C3C", "#F39C12"), domain = names(cities))
  
  # Geography - Map City
  output$mapCity <- renderLeaflet({
    base <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$Stamen.TonerLabels)
    
    if (input$city == "Sri Lanka (All)") {
      # Show full island with city markers only
      base %>% 
        setView(80.77, 7.87, 7) %>%
        addCircleMarkers(
          data = do.call(rbind, lapply(names(cities), function(n) 
            data.frame(name = n, lat = cities[[n]]$coords[1], lng = cities[[n]]$coords[2]))),
          lng = ~lng, lat = ~lat, 
          label = ~name,
          color = ~pal(name), 
          radius = 10, 
          fillOpacity = 0.8,
          stroke = TRUE,
          weight = 2,
          popup = ~paste0("<strong>", name, "</strong><br>Click to explore")
        )
    } else {
      # Zoom into selected city
      cinfo <- cities[[input$city]]
      base %>% 
        setView(cinfo$coords[2], cinfo$coords[1], 13) %>%
        addCircleMarkers(
          lng = cinfo$coords[2], lat = cinfo$coords[1],
          radius = 12, 
          color = pal(input$city), 
          fillOpacity = 0.9,
          stroke = TRUE,
          weight = 2,
          label = input$city,
          popup = paste0("<strong>", input$city, "</strong><br>", cinfo$desc)
        )
    }
  })
  
  # City details
  output$cityImage_ui <- renderUI({
    req(input$city != "Sri Lanka (All)")
    tags$img(src = cities[[input$city]]$image, width = "100%", alt = input$city, 
             style = "border-radius: 8px; box-shadow: 0 2px 5px rgba(0,0,0,0.1);")
  })
  
  output$cityName <- renderText({
    req(input$city != "Sri Lanka (All)")
    input$city
  })
  
  output$cityDesc <- renderText({
    req(input$city != "Sri Lanka (All)")
    cities[[input$city]]$desc
  })
  
  output$cityStats <- renderDT({
    req(input$city != "Sri Lanka (All)")
    datatable(cities[[input$city]]$stats, 
              options = list(dom = 't', searching = FALSE, paging = FALSE),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
  
  # Demographics
  output$ethPie <- renderPlotly({
    plot_ly(datasets$ethn, 
            labels = ~Ethnicity, 
            values = ~Percentage, 
            type = 'pie',
            marker = list(colors = c('#3498DB', '#E74C3C', '#2ECC71', '#F39C12', '#9B59B6')),
            textinfo = 'label+percent',
            hoverinfo = 'text',
            text = ~paste(Ethnicity, "<br>", Percentage, "%"),
            textposition = 'inside') %>%
      layout(title = 'Ethnic Composition', 
             margin = list(l = 50, r = 50, b = 50, t = 50),
             showlegend = TRUE)
  })
  
  output$relBar <- renderPlotly({
    plot_ly(datasets$relig, 
            x = ~Religion, 
            y = ~Percentage, 
            type = 'bar',
            marker = list(color = c('#3498DB', '#E74C3C', '#2ECC71', '#F39C12', '#9B59B6'))) %>%
      layout(title = 'Religious Composition',
             yaxis = list(title = '%', range = c(0, 100)),
             margin = list(l = 50, r = 50, b = 80, t = 50))
  })
  
  output$pyramid_plot <- renderPlot({
    
    df = read_csv("neiss/pop_pyramid.csv")
    # Pivot the data to long format and manipulate it
    # pyramid_data <- df %>%
    #   pivot_longer(cols = c(M, F), names_to = "Sex", values_to = "Population") %>%
    #   mutate(
    #     Population = ifelse(Sex == "M", -Population, Population),  # Invert population for males
    #     Age = factor(Age, levels = unique(df$Age))  # Keep age order as is
    #   )
    
    pyramid_data <- df %>%
      gather(key = "Sex", value = "Population", M, F) %>%
      mutate(
        Population = ifelse(Sex == "M", -Population, Population),
        Age = factor(Age, levels = unique(df$Age))
      )
    
    
    # Create the population pyramid plot
    ggplot(pyramid_data, aes(x = Population, y = Age, fill = Sex)) +
      geom_bar(stat = "identity", width = 0.9) +
      scale_x_continuous(labels = abs) +
      scale_fill_manual(values = c("M" = "#4e79a7", "F" = "#f28e2b"), labels = c("Male", "Female")) +
      labs(x = "Population", y = "Age Group", title = "Population Pyramid - 2024") +
      theme_minimal() +
      theme(
        legend.position = "bottom",
        axis.title = element_text(size = 14),  # Increase size of axis titles
        axis.text = element_text(size = 12),   # Increase size of axis labels
        plot.title = element_text(size = 16, face = "bold"),  # Increase size of plot title
        legend.text = element_text(size = 12),  # Increase size of legend text
        legend.title = element_text(size = 14)  # Increase size of legend title
      )
  })
  
  
  
  # Economy
  output$gdpPlot = renderPlotly({
    ggplotly(
      ggplot(datasets$gdp, aes(Year, GDP_USD_Billions)) + 
        geom_col(fill = '#2980B9') + 
        geom_smooth(method = "loess", color = "#E74C3C", se = FALSE) +
        labs(title = 'GDP Growth Trend', y = 'USD Billions') + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$exportTable <- renderDT({
    datatable(datasets$exports,
              rownames = FALSE,
              options = list(pageLength = 5, dom = 'ft'),
              caption = 'Major Export Commodities',
              class = 'cell-border stripe')
  })
  
  output$exportBar <- renderPlotly({
    ggplotly(
      ggplot(datasets$exports, aes(reorder(Commodity, -Value_USD_Millions), Value_USD_Millions)) + 
        geom_col(fill = '#27AE60') + 
        labs(title = 'Export Value by Commodity', y = 'USD Millions', x = '') + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  output$tourismBar <- renderPlotly({
    ggplotly(
      ggplot(datasets$tour, aes(Year, Arrivals/1e6)) + 
        geom_col(fill = '#9B59B6') + 
        labs(title = 'Tourist Arrivals Over Time', y = 'Visitors (Millions)') + 
        theme_minimal() +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    )
  })
  
  # Environment - FIXED MAP
  output$mapHotspots <- renderLeaflet({

    
    # Create the map with a wider range
    leaflet(data = hotspots) %>%
      addTiles() %>%
      
      # Add markers with labels
      addMarkers(
        lng = ~lng,
        lat = ~lat,
        label = ~name,
        popup = ~paste0("<strong>", name, "</strong><br>Key biodiversity hotspot")
      ) %>%
      
      # Set the view with more visible range by adjusting zoom level and center
      setView(lng = 80.7, lat = 7.2, zoom =7)  # Adjust zoom level and center coordinates
  })
  
  
  
  output$hotspotTable <- renderDT({
    hotspot_info <- data.frame(
      Location = hotspots$name,
      Type = c("Rainforest", "Montane Plateau", "Mountain Range", "National Park", "National Park"),
      Area_km2 = c(88.6, 31.6, 155, 979, 308.2),
      stringsAsFactors = FALSE
    )
    
    datatable(hotspot_info,
              options = list(dom = 't', searching = FALSE, paging = FALSE),
              rownames = FALSE,
              caption = "Protected Areas Information",
              class = 'cell-border stripe')
  })
  
  output$speciesTable <- renderDT({
    species_data <- data.frame(
      Species = c("Sri Lankan Leopard", "Purple-faced Langur", "Sri Lankan Elephant", "Red-faced Malkoha", "Sambar Deer"),
      Scientific_Name = c("Panthera pardus kotiya", "Semnopithecus vetulus", "Elephas maximus maximus", "Phaenicophaeus pyrrhocephalus", "Rusa unicolor"),
      Status = c("Endangered", "Vulnerable", "Endangered", "Least Concern", "Vulnerable"),
      stringsAsFactors = FALSE
    )
    
    datatable(species_data,
              options = list(pageLength = 5, dom = 'ft'),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
  
  # Politics
  prov <- data.frame(
    Province = c("Western", "Central", "Southern", "Northern", "Eastern", 
                 "North Western", "North Central", "Uva", "Sabaragamuwa"),
    Capital = c("Colombo", "Kandy", "Galle", "Jaffna", "Trincomalee", 
                "Kurunegala", "Anuradhapura", "Badulla", "Ratnapura"),
    Population_million = c(5.8, 2.7, 2.5, 1.1, 1.6, 2.4, 1.3, 1.2, 1.9),
    stringsAsFactors = FALSE
  )
  
  output$polTable <- renderDT({
    datatable(prov,
              options = list(dom = 'ft', pageLength = 9, searching = FALSE),
              rownames = FALSE,
              class = 'cell-border stripe')
  })
}

# Run App
shinyApp(ui, server)