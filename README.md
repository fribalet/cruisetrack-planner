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
    - Set the cruise start date and time.
- **Upload/Download Data:**
    - Upload a CSV or TSV file to populate the app with station data.
    - Download the current cruise track data as a CSV file.

## Usage

1. **Launch the App:** Run the `CruiseTrackPlanner.R` script in R or RStudio to launch the Shiny app.
2. **Set Cruise Start:** Enter the cruise start date and time in UTC.
3. **Add Stations:**
    - Select a station name from the dropdown or enter a new one (Station NA).
    - Fill in the details for the station (name, latitude, longitude, ship speed, time on station, etc ...).
    - Choose where to add the station in the cruise track using the "Add After Station" dropdown.
    - Click "Save Station" to save the station.
4. **Edit Stations:**
    - Select an existing station from the "Select Station" dropdown.
    - Modify the station details as needed.
    - Click "Save Station" to save the changes.
5. **Upload Data:**
    - Click "Browse" to select a CSV or TSV file containing station data.
    - The app will automatically populate the map and table with the uploaded data.
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

- Add functionality to delete stations.
- Implement more advanced map features (e.g., custom basemaps, layers).
- Improve error handling and validation.