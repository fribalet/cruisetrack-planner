library(shiny)
library(leaflet)
library(geosphere)


#---
# UI
#---
ui <- fluidPage(
  
  # Application title
  titlePanel("Cruise Track Planner"),
  
  sidebarLayout(
    sidebarPanel(
      # Cruise start date and time input in a separate box
      wellPanel(
        fileInput("uploadData", "Upload Cruise Track", accept = c(".csv", ".tsv")),
        textInput("cruiseStartTime", "Cruise Start Time (UTC):", value = format(Sys.time(), "%Y-%m-%dT%H:%M", tz = "UTC")),
        selectInput("Station", "Select Station:", choices = NULL), 
        p("Select `Station NA` to add a new station to the uploaded cruise track.")
        ),
      
      # Other input elements in another box
      wellPanel(
        textInput("stationName", "Edit Name:", ""), 
        numericInput("Latitude", "Latitude (ºN):", value = 0, min = -90, max = 90),
        numericInput("Longitude", "Longitude (ºE):", value = 0, min = -180, max = 180),
        textInput("Operations", "Operations:", ""),
        numericInput("ShipSpeed", "Ship Speed (knots):", value = 10, min = 0),
        numericInput("TimeOnStation", "Time on Station (hours):", value = 24, min = 0),
        selectInput("addAfterStation", "Add After Station:", choices = NULL), 
        actionButton("addStation", "Save Station")
        ),
      
      # Cruise start date and time input in a separate box
      wellPanel(
        downloadButton("downloadTable", "Download Table") 
      ),
      
    ),
    
    mainPanel(
      leafletOutput("map"),
      tableOutput("stationTable")
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
    newStation <- ifelse(is.null(stationData$name), "Station 1", 
                             paste("Station", as.numeric(gsub("Station ", "", tail(stationData$name, 1))) + 1))
    updateSelectInput(session, "Station", choices = c(newStation, stationData$name))
    updateSelectInput(session, "addAfterStation", choices = c("End", stationData$name))
  })
  
  
  # Update input fields when "Select Station" is changed
  observeEvent(input$Station, {
    selectedStation <- input$Station
    
    # Update stationName textInput (for both existing and new stations)
    updateTextInput(session, "stationName", value = selectedStation) 
    
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
        # If it's the first station, set "Add After Station" to "End"
        updateSelectInput(session, "addAfterStation", selected = "End") 
      }
    } else {
      # Reset input fields if a new station is selected
      updateNumericInput(session, "Latitude", value = 0)
      updateNumericInput(session, "Longitude", value = 0)
      updateTextInput(session, "Operations", value = "")
      updateNumericInput(session, "ShipSpeed", value = 10)
      updateNumericInput(session, "TimeOnStation", value = 24)
      
      # Set "Add After Station" to "End" for new stations
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
      # Calculate addAfterIndex
      if (input$addAfterStation == "End") {
        addAfterIndex <- length(stationData$name) + 1 
      } else {
        addAfterIndex <- which(stationData$name == input$addAfterStation) + 1
      }
      # Insert new station data at the correct index
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
        p1 = cbind(stationData$lon[-length(stationData$lon)], 
                   stationData$lat[-length(stationData$lat)]),
        p2 = cbind(stationData$lon[-1], stationData$lat[-1])
      ) / 1852 # Convert meters to nautical miles
    } else {
      NULL
    }
  })
  
  travelTimes <- reactive({
    if (length(distances()) > 0) { # Check if distances() is not NULL
      distances() / stationData$speed[-1] * 60 # Travel time in minutes
    } else {
      NULL
    }
  })
  
  arrivalTimes <- reactive({
    if (length(stationData$lat) > 1) {
      # Combine date and time input for cruise start
      cruiseStartDateTime <- as.POSIXct(paste(input$cruiseStartDate, input$cruiseStartTime), 
                                        format = "%Y-%m-%dT%H:%M")
      
      # Initialize arrival time with the cruise start time
      arrivalTimes <- c(cruiseStartDateTime) 
      
      # Calculate arrival times relative to cruise start
      if (length(stationData$time) > 1) {
        for (i in 2:length(stationData$time)) {
          prevDeparture <- arrivalTimes[i-1] + stationData$time[i-1] * 60 * 60 # Departure from previous station
          travelTime <- travelTimes()[i-1] * 60 # Travel time to this station in seconds
          arrivalTimes[i] <- prevDeparture + travelTime 
        }
      }
      arrivalTimes
    } else {
      NULL
    }
  })
  
  departureTimes <- reactive({
    if (length(arrivalTimes()) > 0) { # Check if arrivalTimes() is not NULL
      arrivalTimes() + difftime(stationData$time * 60 * 60, 0, units = "secs")
    } else {
      NULL
    }
  })
  
  
  # Create a reactive expression to hold the table data
  tableData <- reactive({
    if (length(stationData$name) > 0) {
      # Combine date and time input for cruise start
      cruiseStartDateTime <- as.POSIXct(paste(input$cruiseStartDate, input$cruiseStartTime), 
                                        format = "%Y-%m-%dT%H:%M")
      
      # Calculate arrival and departure times for the first station
      firstStationArrival <- cruiseStartDateTime 
      firstStationDeparture <- firstStationArrival + stationData$time[1] * 60
      
      # Create a data frame with initial values for the first station
      df <- data.frame(
        Station = stationData$name,
        Latitude = stationData$lat,
        Longitude = stationData$lon,
        ShipSpeed = stationData$speed,
        TimeOnStation = stationData$time,
        Arrival = c(format(firstStationArrival, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station arrival
        Departure = c(format(firstStationDeparture, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station departure
        Distance = 0,
        TravelTime = 0,
        Operations = stationData$operations
      )
      
      # Add data for subsequent stations if they exist
      if (length(stationData$name) > 1) {
        df$Arrival[-1] <- format(arrivalTimes()[-1], "%Y-%m-%dT%H:%M")
        df$Departure[-1] <- format(departureTimes()[-1], "%Y-%m-%dT%H:%M")
        df$Distance[-1] <- distances()
        df$TravelTime[-1] <- travelTimes()
      }
      
      df
    }
  })
  
  
  # Create the table
  output$stationTable <- renderTable({
    if (length(stationData$name) > 0) {
      # Combine date and time input for cruise start
      cruiseStartDateTime <- as.POSIXct(paste(input$cruiseStartDate, input$cruiseStartTime), 
                                        format = "%Y-%m-%dT%H:%M")
      
      # Calculate arrival and departure times for the first station
      firstStationArrival <- cruiseStartDateTime 
      firstStationDeparture <- firstStationArrival + stationData$time[1] * 60
      
      # Create a data frame with initial values for the first station
      df <- data.frame(
        Station = stationData$name,
        Latitude = stationData$lat,
        Longitude = stationData$lon,
        Arrival = c(format(firstStationArrival, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station arrival
        Departure = c(format(firstStationDeparture, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station departure
        Distance = 0,
        TravelTime = 0,
        Operations = stationData$Operations
      )
      
      # Add data for subsequent stations if they exist
      if (length(stationData$name) > 1) {
        df$Arrival[-1] <- sapply(arrivalTimes(), function(x) {
          format(as.POSIXct("1970-01-01 00:00", tz = "UTC") + x * 60, "%Y-%m-%dT%H:%M")
        })
        df$Departure[-1] <- sapply(departureTimes(), function(x) {
          format(as.POSIXct("1970-01-01 00:00", tz = "UTC") + x * 60, "%Y-%m-%dT%H:%M")
        })
        df$Distance[-1] <- distances()
        df$TravelTime[-1] <- travelTimes()
      }
      
      df
    }
  })
  
  # Upload the table and update reactive values, map, and table
  observeEvent(input$uploadData, {
    req(input$uploadData)
    tryCatch(
      {
        df <- read.csv(input$uploadData$datapath)
        # Update reactive values with uploaded data
        stationData$name <- df$Station
        stationData$lat <- df$Latitude
        stationData$lon <- df$Longitude
        stationData$speed <- df$ShipSpeed
        stationData$time <- df$TimeOnStation        
        stationData$Operations <- df$Operations
        
        # Trigger table and map updates
        tableData() 
        invalidateLater(0, session) 
        
        # Update cruise start date and time from the uploaded table
        if ("Arrival" %in% names(df) && !is.na(df$Arrival[1])) {
          arrivalTime <- as.POSIXct(df$Arrival[1], format = "%Y-%m-%dT%H:%M", tz = "UTC")
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
    leaflet() %>% addTiles()
    
    # Add markers only if there are stations
    if (length(stationData$lat) > 0) {
      leaflet() %>%
        addTiles() %>%
        addMarkers(
          lng = stationData$lon, 
          lat = stationData$lat, 
          popup = paste(
            "<strong>Station:</strong>", stationData$name, "<br>",
            "<strong>Operations:</strong>", stationData$Operations # Add Operations to popup
          )
        )
    }
    
    if (length(stationData$lat) > 1) {
      leaflet() %>%
        addTiles() %>%
        addPolylines(lng = stationData$lon, lat = stationData$lat) %>%
        addMarkers(
          lng = stationData$lon, 
          lat = stationData$lat, 
          popup = paste(
            "<strong>Station:</strong>", stationData$name, "<br>",
            "<strong>Operations:</strong>", stationData$Operations # Add Operations to popup
          )
        )
    }
  })
  
  # Calculate distances and times (moved outside observeEvent)
  distances <- reactive({
    if (length(stationData$lat) > 1) {
      distGeo(
        p1 = cbind(stationData$lon[-length(stationData$lon)], 
                   stationData$lat[-length(stationData$lat)]),
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
      cruiseStartDateTime <- as.POSIXct(paste(input$cruiseStartDate, input$cruiseStartTime), 
                                        format = "%Y-%m-%dT%H:%M")
      
      # Initialize arrival time with the cruise start time
      arrivalTimes <- c(cruiseStartDateTime) 
      
      # Calculate arrival times for subsequent stations
      if (length(stationData$time) > 1) {
        for (i in 2:length(stationData$time)) {
          prevDeparture <- arrivalTimes[i-1] + stationData$time[i-1] * 60 * 60 # Departure from previous station
          travelTime <- travelTimes()[i-1] * 60 # Travel time to this station in seconds
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
      # Combine date and time input for cruise start
      cruiseStartDateTime <- as.POSIXct(paste(input$cruiseStartDate, input$cruiseStartTime), 
                                        format = "%Y-%m-%dT%H:%M")
      
      # Calculate arrival and departure times for the first station
      firstStationArrival <- cruiseStartDateTime 
      firstStationDeparture <- firstStationArrival + stationData$time[1] * 60
      
      # Create a data frame with initial values for the first station
      df <- data.frame(
        Station = stationData$name,
        Latitude = stationData$lat,
        Longitude = stationData$lon,
        ShipSpeed = stationData$speed,
        TimeOnStation = stationData$time,
        Arrival = c(format(firstStationArrival, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station arrival
        Departure = c(format(firstStationDeparture, "%Y-%m-%dT%H:%M"), rep("", length(stationData$name) - 1)), # Set first station departure
        Distance = 0,
        TravelTime = 0,
        Operations = stationData$Operations
      )
      
      # Add data for subsequent stations if they exist
      if (length(stationData$name) > 1) {
        df$Arrival[-1] <- format(arrivalTimes()[-1], "%Y-%m-%dT%H:%M")
        df$Departure[-1] <- format(departureTimes()[-1], "%Y-%m-%dT%H:%M")
        df$Distance[-1] <- distances()
        df$TravelTime[-1] <- travelTimes()
      }
      
      df
    }
  })
  
  # Render the table using the reactive expression
  output$stationTable <- renderTable({
    tableData()
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

#--------------------
# Run the application
#--------------------
shinyApp(ui = ui, server = server)
