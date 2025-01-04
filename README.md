# Cruise Track Planner

This Shiny app provides a user-friendly interface for planning cruise tracks, allowing users to add stations, visualize the track on a map, and manage the cruise schedule.

## Usage

1. **Launch the App:** Run the `CruiseTrackPlanner.R` script in R or RStudio to launch the Shiny app.
2. **Upload Data (Optional):** 
    -   Under the "Upload Cruise Track (Optional)" section, click "Browse" to select a CSV or TSV file containing station data.
    -   The file must contain the columns: `StationName`, `Latitude`, `Longitude`, `ShipSpeed`, `TimeOnStation`, and `Operations`.
    -   The app will populate the map, table, and input fields with the uploaded data.
3.  **Edit Time Information:**
    -   If you did not upload a cruise track, enter the cruise start date and time in UTC in the "Cruise Start Time (UTC)" field.
    -   Select the time zone for local time from the "Time Zone for Local Time (hours)" dropdown menu.
4.  **Create a New Station (Optional):**
    -   Under the "Create a New Station (Optional)" section, type the name of the new station in the "New Station Name" field.
    -   Select the station after which you want to insert the new station from the "Add After Station" dropdown menu. The default is to add the new station at the end of the cruise track.
5. **Edit Station Field Information**
  -  Select a station from the `Select Station` dropdown menu to view and edit its information.
  - `Edit name`: This field displays the name of the selected station. You can edit this field to change the station name.
  - `Latitude (ºN)`: Enter the latitude of the station in decimal degrees North.
  - `Longitude (ºE)`: Enter the longitude of the station in decimal degrees East.
  - `Operations`: Enter a description of the operations to be conducted at this station (e.g., "CTD cast at 1000 m","Zooplankton tow at 200 m","Shipek at 1000 m").
  - `Ship Speed (knots)`: Enter the planned ship speed in knots for traveling to this station. This will be used to calculate the travel time between stations.
  - `Time on Station (hours)`: Enter the planned time (in hours) that the ship will spend at this station.
  - Click "Save Station" to save the station information.
6. **Download Data:**
  - Click "Download Table" to download the current cruise track data as a CSV file.

## File Structure

- `CruiseTrackPlanner.R`: Contains the R code for the Shiny app (UI and server logic).
- `cruise_track.csv`: This file contains an example cruise track.

## Dependencies

- `shiny`
- `leaflet`
- `geosphere`
- `DT`