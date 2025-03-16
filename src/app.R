library(shiny)
library(bslib)
library(plotly)
chooseCRANmirror(graphics = FALSE, ind = 1)
install.packages("osmdata")
install.packages("sf")
install.packages("leaflet")
library(osmdata)  # For OpenStreetMap data
library(sf)
library(leaflet)
library(dplyr)

# Same port each time and enable hot reloading
options(shiny.port = 8060, shiny.autoreload = TRUE)

# Read in csv
df <- read.csv("data/raw/mental-health.csv")

# Read in the shapefile
bc_shapefile <- st_read("data/raw/BC-map/BC.shp")

# Layout
ui <- page_sidebar(
  theme = bs_theme(bootswatch = "litera"),
  title = "BC Mental Health and Substance Use Health Services",
  sidebar = sidebar(
    h6("Select the type of clinic"),
    checkboxGroupInput("cat_multi",
      NULL,
      choices = c("Alcohol Addictions and Other Substances",
                  "Body Image and Eating", "Education and Awareness",
                  "Mood and Anxiety", "Psychosis and Thought Disorders",
                  "Suicide and Self Harm", "Trauma and Abuse"),
      selected = c("Alcohol Addictions and Other Substances",
                   "Body Image and Eating", "Education and Awareness",
                   "Mood and Anxiety", "Psychosis and Thought Disorders",
                   "Suicide and Self Harm", "Trauma and Abuse")
    ),
    br(),
    h6("Select age range"),
    checkboxGroupInput("age_multi",
      NULL,
      choices = c("Students", "Children and Youth", "Adults"),
      selected = c("Students", "Children and Youth", "Adults")
    ),
  ),
  layout_columns(
    leafletOutput("map"),
    card(h4("Location Details"),
         htmlOutput("location_name"),
         htmlOutput("org"),
         htmlOutput("phone"),
         htmlOutput("website"),
         htmlOutput("wheelchair"),
         htmlOutput("lang"),
         htmlOutput("hrs"),
         textOutput("description")
    ),
    col_widths = c(8, 4)
  )
)

# Server side callbacks/reactivity
server <- function(input, output, session) {
  # Reactive value to store selected location details
  selected_location <- reactiveValues(name = NULL, lat = NULL, lon = NULL)

  filtered_df <- reactive({
    req(input$cat_multi)
    selected_ages <- input$age_multi
    selected_cats <- input$cat_multi
    df |>
      filter(sapply(GPCE_TAXONOMY_CLASSIFICATION, function(x) {
        any(selected_cats %in% unlist(strsplit(x, "\\|")))
      })) |>
      filter(sapply(AUDIENCE, function(x) {
        any(selected_ages %in% unlist(strsplit(x, "\\|")))
      }))
  })
  # Render the map with the shapefile
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$CartoDB.Positron) |>
      addPolygons(data = bc_shapefile, fillColor = "blue", color = "black",
                  weight = 1, opacity = 0, fillOpacity = 0) |>
      addMarkers(data = filtered_df(), lng = ~LONGITUDE, lat = ~LATITUDE,
                 clusterOptions = markerClusterOptions(),
                 popup = ~as.character(SV_NAME),
                 layerId = ~MHSU_GUID)
  })

  # Observe clicks on the map
  observeEvent(input$map_marker_click, {
    click <- input$map_marker_click
    if (!is.null(click)) {
      selected_location$id <- click$id
      clicked_df <- filtered_df() |>
        filter(MHSU_GUID == selected_location$id)
      selected_location$name <- clicked_df |> pull(SV_NAME)
      selected_location$org <- clicked_df |> pull(RG_NAME)
      selected_location$description <- clicked_df |> pull(SV_DESCRIPTION)
      selected_location$phone <- clicked_df |> pull(PHONE_NUMBER)
      selected_location$website <- clicked_df |> pull(WEBSITE)
      selected_location$wheelchair <- clicked_df |> pull(WHEELCHAIR_ACCESSIBLE)
      selected_location$lang <- clicked_df |> pull(LANGUAGE)
      selected_location$hrs <- clicked_df |> pull(HOURS)
      selected_location$street <- clicked_df |> pull(STREET_NUMBER)
      selected_location$city <- clicked_df |> pull(CITY)
      selected_location$province <- clicked_df |> pull(PROVINCE)
      selected_location$postal <- clicked_df |> pull(POSTAL_CODE)
    }
  })

  # Display location details in sidebar
  output$location_name <- renderUI({
    if (!is.null(selected_location$name)) {
      HTML(paste0("<span><B>Name:</B> ", selected_location$name))
    } else {
      "Click a marker to see details"
    }
  })
  output$org <- renderUI({
    if (!is.null(selected_location$org)) {
      HTML(paste0("<span><B>Organization:</B> ", selected_location$org))
    }
  })
  output$description <- renderText({
    if (!is.null(selected_location$description)) {
      selected_location$description
    }
  })
  output$phone <- renderUI({
    if (!is.null(selected_location$phone)) {
      HTML(paste0("<span><B>Phone Number:</B> ", selected_location$phone))
    }
  })
  output$website <- renderUI({
    if (!is.null(selected_location$website)) {
      HTML(paste0("<span><B>Website:</B> <a href=\"",
                  selected_location$website,
                  "\">Link</a>"))
    }
  })
  output$wheelchair <- renderUI({
    if (!is.null(selected_location$wheelchair)) {
      HTML(paste0("<span><B>Wheelchair Accessible:</B> ",
                  selected_location$wheelchair))
    }
  })
  output$lang <- renderUI({
    if (!is.null(selected_location$lang)) {
      HTML(paste0("<span><B>Language(s):</B> ", selected_location$lang))
    }
  })
  output$hrs <- renderUI({
    if (!is.null(selected_location$hrs)) {
      if (selected_location$hrs != "") {
        HTML(paste0("<span><B>Hours of Operation:</B> ", selected_location$hrs))
      }
    }
  })
  output$hrs <- renderUI({
    if (!is.null(selected_location$street)) {
      HTML(paste("<span><B>Address:</B> ", selected_location$street, ", ",
                 selected_location$city, ", ", selected_location$province,
                 ", ", selected_location$postal, sep = ""))
    }
  })
}

# Run the app/dashboard
shinyApp(ui, server)