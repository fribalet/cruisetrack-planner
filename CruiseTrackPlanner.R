library(shiny)
library(leaflet)
library(geosphere)

# Get the current time in the current timezone
local_time <- as.POSIXlt(Sys.time(), tz = Sys.timezone())

# Convert the current time to UTC
utc_time <- as.POSIXct(format(local_time, tz="UTC"), tz="UTC")

# Calculate the difference in hours between the current time and UTC
tz_offset <- round(as.numeric(difftime(local_time, utc_time)))
timezone_offset <- ifelse(tz_offset > 0, paste0("+", tz_offset), tz_offset)

# Create the sequence of timezone offsets
tz_sequence <- seq(-12, 14, by = 1)

# Format the sequence with "+" for positive numbers
timezone_sequence <- ifelse(tz_sequence > 0, paste0("+", tz_sequence), tz_sequence)



#---
# UI
#---
ui <- fluidPage(
  
  # Application title
  titlePanel("Cruise Track Planner"),
  
  sidebarLayout(
    sidebarPanel(
      # Cruise start date and time input
      wellPanel(
        fileInput("uploadData", "Upload Cruise Track (Optional)", accept = c(".csv", ".tsv")),
        textInput("cruiseStartTime", "Cruise Start Time (UTC):", value = format(Sys.time(), "%Y-%m-%dT%H:%M", tz = "UTC")),
        selectInput("timeZoneOffset", "Time Zone for Local Time (hours):", choices = timezone_sequence, selected = timezone_offset)
      ),
      
      # Station input and editing elements
      wellPanel(
        p("Add a new station or select an existing to edit its fields"),
        selectInput("Station", "Select Station:", choices = NULL),
        p("Editable fields"),
        textInput("stationName", "Station Name:", ""), 
        numericInput("Latitude", "Latitude (decimal ºN):", value = 0, min = -90, max = 90),
        numericInput("Longitude", "Longitude (decimal ºE):", value = 0, min = -180, max = 180),
        textInput("Operations", "Operations:", ""),
        numericInput("ShipSpeed", "Ship Speed (knots):", value = 12, min = 0),
        numericInput("TimeOnStation", "Time on Station (hours):", value = 12, min = 0),
        p("Choose where to insert the new station in the cruise track (default is at the end)"),
        selectInput("addAfterStation", "Add After Station:", choices = NULL),
        p("Click the button to save the station"),
        actionButton("addStation", "Save Station")
      ),
      
      # Download button
      wellPanel(
        p("Download the cruise track as a CSV file"),
        downloadButton("downloadTable", "Download Table") 
      )
      
    ),
    
    mainPanel(
      leafletOutput("map"),
      tableOutput("stationTable")
    )
  ),
  
  # Footer with author and license information
  hr(),  # Add a horizontal line separator
  fluidRow(
    column(12, 
           p("© 2024 Francois Ribalet. This work is licensed under CC BY NC ND 4.0"), 
           align = "center"  # Center the text
    )
  )
)

#-------
# SERVER
#-------
server <- function(input, output, session) {
  
  # Reactive values to store station data
  stationData <- reactiveValues(
    name = NULL,
    lat = NULL,
    lon = NULL,
    speed = NULL,
    time = NULL,
    Operations = NULL
  )
  
  # Update choices for "Station Name" and "Add After Station" dropdowns
  observe({
    # Determine the new station name
    if (!is.null(stationData$name) && length(stationData$name) > 0) {
      lastStationNumber <- suppressWarnings(as.numeric(gsub("Station ", "", tail(stationData$name, 1))))
      if (!is.na(lastStationNumber)) {
        newStation <- paste("Station", lastStationNumber + 1)
      } else {
        newStation <- "Add a new station" 
      }
    } else {
      newStation <- "Station 1" # Default to Station 1 if no stations
    }
    
    updateSelectInput(session, "Station", choices = c(newStation, stationData$name))
    updateSelectInput(session, "addAfterStation", choices = c("End", stationData$name))
  })
  
  
  # Update input fields when "Select Station" is changed
  observeEvent(input$Station, {
    selectedStation <- input$Station
    
    # Update stationName textInput
    updateTextInput(session, "stationName", value = ifelse(selectedStation == "Add a new station", "New Station", selectedStation))
    
    if (!is.null(selectedStation) && selectedStation %in% stationData$name) {
      stationIndex <- which(stationData$name == selectedStation)
      updateNumericInput(session, "Latitude", value = stationData$lat[stationIndex])
      updateNumericInput(session, "Longitude", value = stationData$lon[stationIndex])
      updateTextInput(session, "Operations", value = stationData$Operations[stationIndex])
      updateNumericInput(session, "ShipSpeed", value = stationData$speed[stationIndex])
      updateNumericInput(session, "TimeOnStation", value = stationData$time[stationIndex])
      
      # Automatically select the previous station in "Add After Station"
      if (stationIndex > 1) {
        previousStation <- stationData$name[stationIndex - 1]
        updateSelectInput(session, "addAfterStation", selected = previousStation)
      } else {
        updateSelectInput(session, "addAfterStation", selected = "End") 
      }
    } else {
      # Reset input fields for a new station
      updateNumericInput(session, "Latitude", value = 0)
      updateNumericInput(session, "Longitude", value = 0)
      updateTextInput(session, "Operations", value = "")
      updateNumericInput(session, "ShipSpeed", value = 10)
      updateNumericInput(session, "TimeOnStation", value = 24)
      
      updateSelectInput(session, "addAfterStation", selected = "End") 
    }
  })
  
  
  # Add/Edit station when button is clicked
  observeEvent(input$addStation, {
    if (input$Station %in% stationData$name) {
      # Edit existing station
      stationIndex <- which(stationData$name == input$Station)
      stationData$name[stationIndex] <- input$stationName
      stationData$lat[stationIndex] <- input$Latitude
      stationData$lon[stationIndex] <- input$Longitude
      stationData$Operations[stationIndex] <- input$Operations
      stationData$speed[stationIndex] <- input$ShipSpeed
      stationData$time[stationIndex] <- input$TimeOnStation
    } else {
      # Add new station
      addAfterIndex <- ifelse(input$addAfterStation == "End", length(stationData$name) + 1, which(stationData$name == input$addAfterStation) + 1)
      
      # Insert new station data
      stationData$name <- append(stationData$name, input$stationName, after = addAfterIndex - 1)
      stationData$lat <- append(stationData$lat, input$Latitude, after = addAfterIndex - 1)
      stationData$lon <- append(stationData$lon, input$Longitude, after = addAfterIndex - 1)
      stationData$Operations <- append(stationData$Operations, input$Operations, after = addAfterIndex - 1)
      stationData$speed <- append(stationData$speed, input$ShipSpeed, after = addAfterIndex - 1)
      stationData$time <- append(stationData$time, input$TimeOnStation, after = addAfterIndex - 1)
    }
  })
  
  
  # Calculate distances and times
  distances <- reactive({
    if (length(stationData$lat) > 1) {
      distGeo(
        p1 = cbind(stationData$lon[-length(stationData$lon)], stationData$lat[-length(stationData$lat)]),
        p2 = cbind(stationData$lon[-1], stationData$lat[-1])
      ) / 1852 # Convert meters to nautical miles
    } else {
      NULL
    }
  })
  
  travelTimes <- reactive({
    if (length(distances()) > 0) { 
      distances() / stationData$speed[-1] * 60 # Travel time in minutes
    } else {
      NULL
    }
  })
  
  # Calculate arrival times taking departure times into account
  arrivalTimes <- reactive({
    if (length(stationData$lat) > 1) {
      arrivalTimes <- as.POSIXct(input$cruiseStartTime, format = "%Y-%m-%dT%H:%M", tz = "UTC")
      
      if (length(stationData$time) > 1) {
        for (i in 2:length(stationData$time)) {
          prevDeparture <- arrivalTimes[i-1] + stationData$time[i-1] * 60 * 60 
          travelTime <- travelTimes()[i-1] * 60 
          arrivalTimes[i] <- prevDeparture + travelTime 
        }
      }
      arrivalTimes
    } else {
      NULL
    }
  })
  
  # Calculate departure times based on arrival times
  departureTimes <- reactive({
    if (length(arrivalTimes()) > 0) {
      arrivalTimes() + stationData$time * 60 * 60
    } else {
      NULL
    }
  })
  
  # Create a reactive expression to hold the table data
  tableData <- reactive({
    if (length(stationData$name) > 0) {
      # Calculate arrival and departure times for the first station
      firstStationArrival <- as.POSIXct(input$cruiseStartTime, format = "%Y-%m-%dT%H:%M")
      firstStationDeparture <- firstStationArrival + stationData$time[1] * 60 * 60
      
      # Create a data frame with initial values
      df <- data.frame(
        Station = stationData$name,
        Latitude = stationData$lat,
        Longitude = stationData$lon,
        ShipSpeed = stationData$speed,
        TimeOnStation = stationData$time,
        ArrivalUTC = c(format(firstStationArrival, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), 
        DepartureUTC = c(format(firstStationDeparture, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), 
        Distance = 0,
        TravelTime = 0,
        Operations = stationData$Operations
      )
      
      # Add data for subsequent stations
      if (length(stationData$name) > 1) {
        df$ArrivalUTC[-1] <- format(arrivalTimes()[-1], "%Y-%m-%dT%H:%M")
        df$DepartureUTC[-1] <- format(departureTimes()[-1], "%Y-%m-%dT%H:%M")
        df$Distance[-1] <- distances()
        df$TravelTime[-1] <- travelTimes()
      }
      
      # Ensure UTC timezone and correct formatting
      df$ArrivalUTC <- as.POSIXct(df$ArrivalUTC, format = "%Y-%m-%dT%H:%M", tz = "UTC")
      df$DepartureUTC <- as.POSIXct(df$DepartureUTC, format = "%Y-%m-%dT%H:%M", tz = "UTC")
      
      # Calculate local arrival and departure times
      df$ArrivalLocal <- format(df$ArrivalUTC, format = "%Y-%m-%dT%H:%M",tz = paste0("Etc/GMT", input$timeZoneOffset))
      df$DepartureLocal <- format(df$DepartureUTC, format = "%Y-%m-%dT%H:%M", tz = paste0("Etc/GMT", input$timeZoneOffset))
      
      # Convert back ArrivalUTC and DepartureUTC to correct formating
      df$ArrivalUTC <- format(df$ArrivalUTC, format = "%Y-%m-%dT%H:%M", tz = "UTC")
      df$DepartureUTC <- format(df$DepartureUTC, format = "%Y-%m-%dT%H:%M", tz = "UTC")
      
      df
    }
  })
  
  # Render the table 
  output$stationTable <- renderTable({
    tableData()
  })
  
  # Upload the table and update data
  observeEvent(input$uploadData, {
    req(input$uploadData)
    tryCatch(
      {
        df <- read.csv(input$uploadData$datapath)
        
        # Update reactive values
        stationData$name <- df$Station
        stationData$lat <- df$Latitude
        stationData$lon <- df$Longitude
        stationData$speed <- df$ShipSpeed
        stationData$time <- df$TimeOnStation        
        stationData$Operations <- df$Operations
        
        # Update cruise start time 
        if ("ArrivalUTC" %in% names(df) && !is.na(df$ArrivalUTC[1])) {
          arrivalTime <- as.POSIXct(df$ArrivalUTC[1], format = "%Y-%m-%dT%H:%M", tz = "UTC")
          updateTextInput(session, "cruiseStartTime", value = format(arrivalTime, "%Y-%m-%dT%H:%M"))
        }
      },
      error = function(e) {
        showModal(modalDialog(
          title = "Error Reading File",
          "There was an error reading the uploaded file. Please make sure it is a valid CSV or TSV file with the correct columns.",
          easyClose = TRUE
        ))
      }
    )
  })
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      # Add markers and polylines if there are stations
      {if (length(stationData$lat) > 0) addMarkers(.,
                                                   lng = stationData$lon, 
                                                   lat = stationData$lat, 
                                                   popup = paste(
                                                     "<strong>Station:</strong>", stationData$name, "<br>",
                                                     "<strong>Operations:</strong>", stationData$Operations 
                                                   )
      )} %>%
      {if (length(stationData$lat) > 1) addPolylines(., lng = stationData$lon, lat = stationData$lat)}
  })
  
  # Download the table
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("cruise_track-", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tableData(), file, row.names = FALSE)
    }
  )
  
}

# Run the application
shinyApp(ui = ui, server = server)