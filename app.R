library(shiny)
library(bslib)
library(jsonlite, warn.conflicts = FALSE)
library(dplyr, warn.conflicts = FALSE)
library(DT, warn.conflicts = FALSE)
library(maps)
library(mapproj)
library(leaflet)
library(httr)
library(ggmap)


readRenviron(".Renviron")

ui <- fluidPage(
  theme = bs_theme(version = 4, bootswatch = "minty"),
  navbarPage("Yelp",
    inverse = TRUE,

    # Map Tab
    tabPanel(
      "Map Search",
      sidebarLayout(
        sidebarPanel(
          p("Tell Yelp Where You Want To Go"),
          hr(),
          textInput("search_input", "Find"),
          textInput("location_input", "Near"),

          radioButtons("price_filter", label = "Select Prices", choices = list("No Preference" = "", "$", "$$", "$$$", "$$$$")),
          actionButton("map_button", label = "", icon = shiny::icon("search"))
        ),

        # outputs the map
        mainPanel(
          textOutput("map_title"),
          leafletOutput("myMap", height = "700")
        )
      )
    ),

    # Search Tab
    tabPanel(
      "Business Search",
      sidebarLayout(
        sidebarPanel(
          p("Tell Yelp What You Want To Find"),
          hr(),
          textInput("find_input", "Find"),
          textInput("near_input", "Near"),
          actionButton("search_button", label = "", icon = shiny::icon("search"))
        ),
        # outputs table search
        mainPanel(
          dataTableOutput("business_search")
        )
      )
    )
  )
)

server <- function(input, output) {
  yelp_url <- "https://api.yelp.com/v3/businesses/search"
  getData <- function(yelp_query) {
    response <- GET(
      url = paste(yelp_url, sep = ""),
      query = yelp_query,
      add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN"))),
      content_type_json()
    )
    body <- content(response, "text", encoding = "UTF-8")
    data <- fromJSON(body)
    return(data)
  }

  #### SEARCH TAB
  observeEvent(input$search_button, {
    yelp_query1 <- list(term = input$find_input, location = input$near_input, limit = 50)
    yelp_data1 <- getData(yelp_query1)

    # remove unnecessary data
    table <- flatten(yelp_data1[[1]]) %>% select(-id, -is_closed, -categories, -location.display_address, -transactions, -coordinates.latitude, -coordinates.longitude, -distance, -phone)

    # generate link
    table$url <- paste0("<a href='", table$url, "' class = 'button'>Link</a>")

    # generate picture
    table$image_url <- paste("<img src='", table$image_url, "' height = '70'</img>", sep = "")

    # merge all address into one
    table$address <- paste0(table$location.address1, ",", table$location.city, ", ", table$location.state, ", ", table$location.zip_code)

    # remove extra address
    table <- select(table, -alias, -location.address1, -location.address2, -location.address3, -location.city, -location.state, -location.zip_code, -location.country)

    # change name to column titles
    colnames(table) <- c("Name", "Picture", "Link", "Review", "Rating", "Price", "Phone", "Address")

    # render data table
    output$business_search <- renderDataTable(DT::datatable(table, escape = FALSE, selection = "none"))
  })


  ########################################

  #### MAP TAB

  # preps variables that will be used later for plotting
  yelp_df <- data.frame()
  center <- vector("list")

  # create a default map
  map <- leaflet() %>%
    addTiles() %>%
    setView(-95, 42, zoom = 4)
  output$myMap <- renderLeaflet(map)

  output$map_title <- renderText({
    "Yelp Map"
  })


  # observe input
  observeEvent(input$map_button, {
    yelp_query2 <- list(term = input$search_input, location = input$location_input)
    yelp_data2 <- getData(yelp_query2)

    # region & center
    region <- yelp_data2[[3]]
    center <- region[[1]]

    # flatten the data frame
    yelp_df <- flatten(yelp_data2[[1]])

    # when data frame is empty, maps still shows
    if (nrow(yelp_df) == 0) {
      map_zoom <- geocode(input$location_input)
      output$myMap <- renderLeaflet(map %>% setView(map_zoom[[1]], map_zoom[[2]], zoom = 10))
    } else {
      output$myMap <- renderLeaflet(map %>%
        setView(center[[1]], center[[2]], zoom = 10) %>%
        addAwesomeMarkers(
          lng = yelp_df$coordinates.longitude,
          lat = yelp_df$coordinates.latitude, icon = icons, label = yelp_df$name
        ))
    }


    # filter price
    if (input$price_filter != "") {
      yelp_df <- filter(yelp_df, price == input$price_filter)
    }

    # create color codes for rating
    rating_color <- function(yelp_df) {
      sapply(yelp_df$rating, function(rating) {
        if (rating >= 4) {
          "lightgreen"
        } else if (rating >= 3) {
          "orange"
        } else {
          "pink"
        }
      })
    }

    # create icons for map
    icons <- awesomeIcons(
      icon = "ios-close",
      iconColor = "black",
      library = "ion",
      markerColor = rating_color(yelp_df)
    )
  })
}

# Run the application
shinyApp(ui = ui, server = server)
