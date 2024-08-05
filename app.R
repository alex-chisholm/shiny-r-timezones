library(shiny)
library(shinyTime)
library(lubridate)
library(bslib)

# List of timezones (New York removed)
default_timezones <- c(
  "Europe/London",
  "Europe/Istanbul",
  "Asia/Singapore",
  "Australia/Sydney"
)

ui <- page_fluid(
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  
  tags$head(
    tags$style(HTML("
      .card-body .dropdown-menu {
        z-index: 1050 !important;
        max-height: 300px;
        overflow-y: auto;
      }
      .card {
        height: 800px;
      }
      .card-body {
        overflow-y: auto;
      }
    "))
  ),
  
  h1("International Meeting Time Converter"),
  
  fluidRow(
    column(6,
           card(
             card_header("Select Time and Timezone"),
             card_body(
               timeInput("selected_time", "Select Time:", value = NULL, seconds = FALSE),
               selectizeInput("selected_timezone", "Select Timezone:", 
                              choices = OlsonNames(), 
                              selected = NULL,
                              options = list(
                                placeholder = 'Search for a timezone',
                                onInitialize = I('function() { this.setValue(""); }')
                              ))
             )
           )
    ),
    column(6,
           card(
             card_header("Equivalent Times"),
             card_body(
               tableOutput("time_table"),
               hr(),
               h4("Add Custom City"),
               selectizeInput("new_city_timezone", "Select City Timezone:", 
                              choices = OlsonNames(),
                              options = list(
                                placeholder = 'Search for a timezone',
                                onInitialize = I('function() { this.setValue(""); }')
                              )),
               actionButton("add_city", "Add City", class = "btn-primary")
             )
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
    # Combine default and additional timezones
    all_timezones <- c(default_timezones, names(additional_cities()))
    
    result <- data.frame(
      City = gsub(".*/(.*)", "\\1", all_timezones),
      Time = rep("--:-- --", length(all_timezones))
    )
    
    if (!is.null(input$selected_time) && !is.null(input$selected_timezone) && input$selected_timezone != "") {
      # Convert the selected time to POSIXct
      selected_time <- as.POSIXct(input$selected_time, format = "%H:%M")
      
      # Get the current date in the selected timezone
      current_date <- as.Date(with_tz(Sys.time(), input$selected_timezone))
      
      # Combine the current date with the selected time
      selected_datetime <- as.POSIXct(paste(current_date, format(selected_time, "%H:%M")),
                                      tz = input$selected_timezone)
      
      result$Time <- sapply(all_timezones, function(tz) {
        format(with_tz(selected_datetime, tz), "%I:%M %p")
      })
    }
    
    result
  })
}

shinyApp(ui, server)
