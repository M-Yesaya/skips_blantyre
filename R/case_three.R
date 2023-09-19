
library(tidyverse)#data manipulation
library(cluster) #clustering
library(leaflet)#fitting OSM data
# -------------------------------------------------------------------------

skip_waste<- read_csv("waste.csv")

skip_yesaya <- read_csv("wasteskipsblantyre.csv")

# -------------------------------------------------------------------------
# renaming
waste_derived<-skip_waste |>
    rename(skip_name=`Location of the Skip`) |>
    rename(lat=`Geolacation - Latitude`) |>
    rename(long=`Geolacation - Longitude`)

yesaya_derived<-skip_yesaya |>
    rename(skip_name=name)
# -------------------------------------------------------------------------
#merge to have 1 data set based on longitude and latitude
df<-merge(x=yesaya_derived,y=waste_derived,
          by=c("lat","long",'skip_name'), all=TRUE) |> as_tibble()

# -------------------------------------------------------------------------

# Select relevant columns for clustering
df_cluster <- df %>%
    select(skip_name, lat, long) %>%
    na.omit()  # Remove rows with missing values

glimpse(df_cluster)

# Define a function to find close points and choose a representative name
find_close_points <- function(data, tolerance_distance = 0.01) {
    data %>%
        group_by(lat_rounded = round(lat, 2), long_rounded = round(long, 2)) %>%
        summarize(
            skip_name = first(skip_name),
            lat = mean(lat),
            long = mean(long)
        ) %>%
        ungroup() %>%
        select(skip_name, lat, long)
}

# Apply the function to your data
consolidated_data <- find_close_points(df)

# -------------------------------------------------------------------------
# Create a leaflet map
m <- leaflet() %>%
    addTiles()  # Add the default OSM tiles

# Add markers for each coordinate point
m <- m %>%
    addMarkers(
        data = consolidated_data,
        lat = ~lat,
        lng = ~long,
        popup = ~skip_name
    )

# Print the map
m

# 'consolidated_data' now contains the representative skip names, latitudes, and longitudes

# -------------------------------------------------------------------------
#52 skips shown
