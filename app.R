library(shiny)
library(shinyTime)
library(lubridate)
library(bslib)

# List of timezones
default_timezones <- c(
  "America/New_York",
  "Europe/London",
  "Europe/Istanbul",
  "Asia/Singapore",
  "Australia/Sydney"
)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  h1("International Meeting Time Converter"),
  
  card(
    card_header("Select Time and Timezone"),
    card_body(
      fluidRow(
        column(4, timeInput("selected_time", "Select Time:", value = Sys.time(), seconds = FALSE)),
        column(8, selectInput("selected_timezone", "Select Timezone:", 
                              choices = OlsonNames(), 
                              selected = "America/New_York"))
      )
    )
  ),
  
  card(
    card_header("Equivalent Times"),
    card_body(
      tableOutput("time_table")
    )
  ),
  
  card(
    card_header("Add Custom City"),
    card_body(
      fluidRow(
        column(6, selectInput("new_city_timezone", "Select City Timezone:", 
                              choices = OlsonNames())),
        column(6, actionButton("add_city", "Add City", class = "btn-primary"))
      )
    )
  )
)

server <- function(input, output, session) {
  
  # Reactive value to store additional cities
  additional_cities <- reactiveVal(list())
  
  observeEvent(input$add_city, {
    new_city <- input$new_city_timezone
    current_cities <- additional_cities()
    if (!(new_city %in% c(default_timezones, names(current_cities)))) {
      current_cities[[new_city]] <- new_city
      additional_cities(current_cities)
    }
  })
  
  output$time_table <- renderTable({
    req(input$selected_time, input$selected_timezone)
    
    # Convert the selected time to POSIXct
    selected_time <- as.POSIXct(input$selected_time, format = "%H:%M")
    
    # Get the current date in the selected timezone
    current_date <- as.Date(with_tz(Sys.time(), input$selected_timezone))
    
    # Combine the current date with the selected time
    selected_datetime <- as.POSIXct(paste(current_date, format(selected_time, "%H:%M")),
                                    tz = input$selected_timezone)
    
    # Combine default and additional timezones
    all_timezones <- c(default_timezones, names(additional_cities()))
    
    result <- data.frame(
      City = c(gsub(".*/(.*)", "\\1", default_timezones), 
               gsub(".*/(.*)", "\\1", names(additional_cities()))),
      Time = sapply(all_timezones, function(tz) {
        format(with_tz(selected_datetime, tz), "%I:%M %p")
      })
    )
    
    result
  })
}

shinyApp(ui, server)
