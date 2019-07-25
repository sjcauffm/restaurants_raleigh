library(tidyverse)
library(ggmap)
library(leaflet)
library(maps)

data <- read.csv("raleigh_restaurants.csv")

## set google API key
register_google("AIzaSyCBvnmwVSw3KCoIuRiBFqH8j8RIZx_1vDc")

data <- data %>%
  unite(col = address, address, locality, sep = ", ")

lat_long <- lapply(data$address, geocode)

locations <- do.call(rbind.data.frame,lat_long)
data$lat <- locations$lat
data$long <- locations$lon

##### MAKING THE MAP
raleigh_basic <- get_map(location = c(lon = -78.6382, lat = 35.8), zoom = 12, maptype = "satellite",
                   source = "google")

restaurant_map <- ggmap(raleigh) + 
  geom_point(aes(x = long, y = lat, colour = cusine), data = data)

##### Making interactive map
map <- map("state", fill = TRUE, plot = FALSE)

raleigh <- leaflet(data) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, popup = paste("Name:", data$name, "<br>",
                                        "Rating:", data$rating, "<br>",
                                        "Reviews:", data$reviews, "<br>",
                                        "Address:", data$address, "<br>",
                                        "Phone Number:", data$phone_number, "<br>",
                                        "Price:", data$price, "<br>",
                                        "Cuisine:", data$cusine))
