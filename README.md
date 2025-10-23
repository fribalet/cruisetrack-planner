# Cruise Track Planner

Tired of using an outdated Excel file to plan your next cruise? We created a user-friendly interface for planning research cruises, allowing scientists to organize the cruise track stations and add/edit stations and manage the cruise schedule. 

## Usage

1. **Launch the App:** Type `Rscript CruiseTrackPlanner.R` or Click the link to launch the Shiny App:
[Cruise Track Planner](http://seaflow.shinyapps.io/cruisetrackplanner).
2. **Upload Cruise Track (Optional):** 
    -   click `Browse` to select a CSV or TSV file containing station data.
    -   The file must contain the columns: `StationName`, `Latitude`, `Longitude`, `ShipSpeed`, `TimeOnStation`, and `Operations`.
    -   The app will populate the map, table, and input fields with the uploaded data.
3.  **Edit Time Information:**
    -   If you did not upload a cruise track, enter the cruise start date and time in UTC in the `Cruise Start Time (UTC)` field.
    -   Select the time zone for local time from the `Time Zone for Local Time (hours)` dropdown menu.
4.  **Create a New Station (Optional):**
    -   type the name of the new station in the `New Station Name` field.
    -   Select the station after which you want to insert the new station from the `Add After Station` dropdown menu. The default is to add the new station at the end of the cruise track.
5. **Edit Station Field Information**
    -  Select a station from the `Select Station` dropdown menu to view and edit its information.
    - `Edit name`: This field displays the name of the selected station. You can edit this field to change the station name.
    - `Latitude (ºN)`: Enter the latitude of the station in decimal degrees North.
    - `Longitude (ºE)`: Enter the longitude of the station in decimal degrees East.
    - `Operations`: Enter a description of the operations to be conducted at this station (e.g., CTD cast at 1000 m,Zooplankton tow at 200 m, Shipek at 1000 m).
    - `Ship Speed (knots)`: Enter the planned ship speed in knots for traveling to this station. This will be used to calculate the travel time between stations.
    - `Time on Station (hours)`: Enter the planned time (in hours) that the ship will spend at this station.
    - Click `Save Station` to save the station information.
6. **Download the Cruise Track Table:**
    - Click `Download .csv file` to download the current cruise track data as a Comma Separated Values (CSV) file.


## Cruise Track Table Columns

The table displayed in the app provides an overview of the cruise track and includes the following columns:

*   **Station:** Name of the station.
*   **Latitude:** Latitude of the station in decimal degrees North.
*   **Longitude:** Longitude of the station in decimal degrees East.
*   **ShipSpeed:** Planned ship speed in knots for traveling to this station.
*   **TimeOnStation:** Planned time spent at the station in hours.
*   **ArrivalUTC:** Arrival time at the station in Coordinated Universal Time (UTC), formatted as `YYYY-MM-DDTHH:MM`.
*   **DepartureUTC:** Departure time from the station in UTC, formatted as `YYYY-MM-DDTHH:MM`.
*   **ArrivalLocal:** Arrival time at the station in local time, formatted as `YYYY-MM-DDTHH:MM`.
*   **DepartureLocal:** Departure time from the station in local time, formatted as `YYYY-MM-DDTHH:MM`.
*   **Distance:** Distance from the previous station in nautical miles.
*   **TravelTime:** Travel time from the previous station in hours.
*   **Operations:** Description of operations to be conducted at the station.

## Dependencies

- `shiny`
- `leaflet`
- `leaflem`
- `geosphere`
- `DT`
