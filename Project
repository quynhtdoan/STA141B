library(dplyr)
library(DT)
library(ggplot2)
#library(maps)
#library(mapproj)
#library(ggmap)
library(leaflet)
library(httr)
library(jsonlite)
library(shiny)
library(bslib)

usethis::edit_r_environ("project")
readRenviron(".Renviron")

ui <- fluidPage(
    theme = bs_theme(version = 4, bootswatch = "minty"),
    navbarPage("Yelp", inverse = TRUE,
               # Creates a tab panel for Business Search
               tabPanel("Business Search",
                        sidebarLayout(
                            sidebarPanel(
                                p("Shows a table of businesses based on your search terms."),
                                hr(),
                                textInput("search_input", "Search Input"),
                                textInput("location_input", "Location Input"),
                                actionButton("search_button", label = "", icon = shiny::icon("search"))
                            ),
                            # outputs the data table of businesses
                            mainPanel(
                                dataTableOutput("businesses")
                            )
                        )
               )
    )
)

server <- function(input, output) {

    base_yelp_url <- "https://api.yelp.com/v3/businesses/search"
    getData <- function(query.params) {
        response <- GET(url = paste(base_yelp_url, sep = ""), query = query.params, add_headers(Authorization = paste("Bearer", Sys.getenv("YELP_TOKEN"))), content_type_json())
        body <- content(response, "text",encoding = "UTF-8")
        data <- fromJSON(body)
        return (data)
    }

    observeEvent(input$search_button, {
        query.params <- list(term = input$search_input, location = input$location_input, limit = 50)
        business_data <- getData(query.params)

        # this line makes it so the data table can be printed without altering the values in these columns
        compress <- flatten(business_data[[1]]) %>% select(-id, -is_closed, -categories, -location.display_address, -categories, -transactions, -coordinates.latitude, -coordinates.longitude, -distance, -phone)
        compress$image_url <- paste("<img src='", compress$image_url, "' height = '60'</img>", sep = "")
        compress$url <- paste0("<a href='", compress$url, "' class = 'button'>Website</a>")

        # combine addresses to make clean looking address column
        compress$address <- paste0(compress$location.address1, "," , compress$location.city, ", ", compress$location.state, ", ", compress$location.zip_code)

        # finally, deletes the extra address columns
        compress <- select(compress,-alias, -location.address1, -location.address2, -location.city, -location.state, -location.zip_code, -location.address3, -location.country)

        # cleaning up column titles:
        colnames(compress) <- c("Name", "Image", "Yelp Link", "Review Count", "Rating", "Price", "Phone", "Address")

        # sends the data table to the output UI, also allows for HTML tags to apply (i.e. <a href>)
        output$businesses <- renderDataTable(DT::datatable(compress, escape = FALSE, selection = "none"))

    })
}

# Run the application
shinyApp(ui = ui, server = server)
