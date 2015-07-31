# Hamaarag samplinmg events viewer app 
# Version 0.1

library(shiny)
library(RSQLite)
library(leaflet)
library(RColorBrewer)
library(magrittr)

# Load data
groups = c(
  "large_mammals",
  "plants_acacia",
  "plants_polygon_quadrats_points",
  "reptiles",
  "small_mammals",
  "butterflies",
  "birds"
)
popups = c(
  "campaign", 
  "year", 
  "season", 
  "unit", 
  "subunit", 
  "site", 
  "factor", 
  "proximity", 
  "habitat")
years = 2012:2014

db = dbConnect(RSQLite::SQLite(), dbname = "T0_2015-07-28.sqlite")
events = dbGetQuery(db, paste0("SELECT * FROM events_birds"))
# dbDisconnect(db)

# UI
ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(top = 20, right = 10,
    selectInput(
      "sel_group", 
      "Group", 
      choices = groups, 
      selected = "large_mammals",
      multiple = FALSE, 
      selectize = TRUE),
    selectInput(
      "sel_year", 
      "Years", 
      choices = 2012:2014, 
      selected = 2012:2014,
      multiple = TRUE, 
      selectize = TRUE),
    selectInput(
      "sel_popup", 
      "Popup", 
      choices = popups, 
      selected = "site",
      multiple = FALSE, 
      selectize = TRUE)
    #,
#     selectInput("colors", "Color Scheme",
#       rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
#     )#,
    # checkboxInput("legend", "Show legend", TRUE)
  )
)

# SERVER
server <- function(input, output, session) {
  
  observe({
    updateSelectInput(
      session, 
      "sel_year",
      "Years", 
      choices = filteredData1()$year %>% unique %>% sort, 
      selected = filteredData1()$year %>% unique
    )})
  
#     observe({
#       updateSelectInput(
#         session, 
#         "sel_popup",
#         "Popup", 
#         choices = filteredData1() %>% colnames %>% sort, 
#         selected = "site"
#       )})
#   
  # Reactive expressions for the data subsetted to what the user selected
  filteredData1 <- reactive({
    dbGetQuery(db, paste0("SELECT * FROM events_", input$sel_group))
  })
  
  filteredData2 <- reactive({
    final = filteredData1()
    final = final[final$year %in% input$sel_year, ]
    final$var = final[, input$sel_popup]
    final
  })
  
  colorpal <- reactive({
    colorFactor("Dark2", filteredData2()$var)
  })

  output$map <- renderLeaflet({
    leaflet(events) %>% 
      addProviderTiles("Esri.WorldTopoMap") %>%
      fitBounds(~min(lon), ~ min(lat), ~ max(lon), ~ max(lat))
  })
  
  observe({
    pal = colorpal()
    
    map = leafletProxy("map", session, data = filteredData2())
    map %>% clearMarkers()
    
    if (nrow(filteredData2()) > 0)
      map %>% addCircleMarkers(
        ~lon, 
        ~lat, 
        color = ~ pal(var),
        popup = ~ paste(var),
        fillOpacity = 0,
        opacity = 1,
        weight = 3
      )
  })
  
  observe({
    pal <- colorpal()
    
    map = leafletProxy("map", session, data = filteredData2())
    map %>% clearControls()
    
    if (nrow(filteredData2()) > 0 & input$sel_popup != "site")
      map %>% addLegend(position = "bottomleft",
        pal = pal, values = ~var
      )
    
    proxy <- leafletProxy("map", data = events)
    
  })
}

shinyApp(server = server, ui = ui)