
library(tidyverse)#data manipulation
library(cluster) #clustering
library(leaflet)#fitting OSM data
# -------------------------------------------------------------------------

skip_waste<- read_csv("raw_data/waste.csv")

skip_yesaya <- read_csv("raw_data/wasteskipsblantyre.csv")

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
          by=c('skip_name'), all=TRUE) |> as_tibble()

# -------------------------------------------------------------------------

# Select relevant columns for clustering
df_cluster <- df %>%
    select(skip_name, lat.x, long.x) %>%
    na.omit()  # Remove rows with missing values

# Calculate Euclidean distance matrix
dist_matrix <- dist(df_cluster[, c("lat.x", "long.x")])

# Perform hierarchical clustering
hc <- hclust(dist_matrix)

# Set a threshold for distance to determine clusters
threshold <- 0.1

# Cut the dendrogram to obtain clusters
clusters <- cutree(hc, h = threshold)

# Add the cluster labels to the original dataset
df_cluster$cluster <- clusters

# Find the centroid coordinates for each cluster
cluster_centroids <- df_cluster %>%
    group_by(skip_name, cluster) %>%
    summarize(avg_lat = mean(lat.x), avg_long = mean(long.x))

# Select one pair of latitude and longitude for each skip name
selected_coordinates <- cluster_centroids %>%
    group_by(skip_name) %>%
    slice(1)  # Select the first coordinate for each skip name

# -------------------------------------------------------------------------
# Create a leaflet map
m <- leaflet() %>%
    addTiles()  # Add the default OSM tiles
# Add markers for each coordinate point
m <- m %>%
    addMarkers(
        data = df_cluster,
        lat = ~lat.x,
        lng = ~long.x,
        popup = ~skip_name
    )
# Print the map
m
# -------------------------------------------------------------------------
#53 skips shown

write_csv(df_cluster, file=here::here("derived_data/export53.csv"))


