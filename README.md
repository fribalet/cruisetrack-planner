# Cruise Track Planner

This Shiny app provides a user-friendly interface for planning cruise tracks, allowing users to add stations, visualize the track on a map, and manage the cruise schedule.

## Features

- **Add Stations:**
    - Input station name, latitude, longitude, operations, ship speed, and time on station.
    - Choose where to insert the new station in the cruise track.
    - Edit existing stations by selecting them from a dropdown menu.
- **Visualize Cruise Track:**
    - Interactive map displays the cruise track with markers for each station.
    - Popups on markers show station details.
- **Manage Cruise Schedule:**
    - Table displays the cruise schedule with arrival and departure times for each station, calculated based on ship speed and time on station.
    - Set the cruise start date and time (defaults to current UTC time).
- **Upload/Download Data:**
    - Upload a CSV or TSV file to populate the app with station data.
    - Download the current cruise track data as a CSV file.

## Usage

1. **Launch the App:** Run the `CruiseTrackPlanner.R` script in R or RStudio to launch the Shiny app.
2. **Upload Data (Optional):** 
  - Click "Browse" to select a CSV or TSV file containing station data. 
  - The app will populate the map, table, and input fields with the uploaded data. The cruise start time will be set to the arrival time of the first station in the file.
3. **Set Cruise Start (If not uploaded):** Enter the cruise start date and time in UTC in the provided field.
4. **Select Station:** Select a station from the dropdown menu to view and edit its information. Select `--` to add a new station to the uploaded cruise track.
5. **Edit Station information**
  - `Edit name`: This field displays the name of the selected station. You can edit this field to change the station name.
  - `Latitude (ºN)`: Enter the latitude of the station in degrees North.
  - `Longitude (ºE)`: Enter the longitude of the station in degrees West.
  - `Operations`: Enter a description of the operations to be conducted at this station (e.g., "CTD cast at 1000 m","Zooplankton tow at 200 m","Shipek at 1000 m").
  - `Ship Speed (knots)`: Enter the planned ship speed in knots for traveling to this station. This will be used to calculate the travel time between stations.
  - `Time on Station (hours)`: Enter the planned time (in hours) that the ship will spend at this station.
  - `Add After Station`: This dropdown menu lets you choose after which existing station you want to add the new station. Select "End" to add the station at the end of the cruise track.
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

## Note

- Ensure the uploaded CSV/TSV file has the correct columns: "Station", "Latitude", "Longitude", "Operations", "Speed", and "TimeOnStation".
- The "Arrival" column in the uploaded file should have the format "%Y-%m-%dT%H:%M" (e.g., "2024-01-03T10:00").
- The time in the "Arrival" column should be in UTC.

## Future Enhancements

- Add local time conversion for the cruise schedule.
- Add functionality to delete stations.
- Implement more advanced map features (e.g., custom basemaps, layers).
- Improve error handling and validation.