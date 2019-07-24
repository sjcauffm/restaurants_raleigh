library(tidyverse)
library(ggmap)
library(leaflet)

data <- read.csv("raleigh_restaurants.csv")

register_google(key = "AIzaSyDabSZ8Cz6CHXoqAb8Tx3UJOeOQvZFkeyE") ## sets google API key

data <- data %>%
  unite(col = address, address, locality, sep = ", ")

lat_long <- lapply(data$address, geocode)

locations <- do.call(rbind.data.frame,lat_long)
data$lat <- locations$lat
data$long <- locations$lon

##### MAKING THE MAP
raleigh <- get_map(location = c(lon = -78.6382, lat = 35.8), zoom = 12, maptype = "satellite",
                   source = "google")

restaurant_map <- ggmap(raleigh) + 
  geom_point(aes(x = long, y = lat, colour = cusine), data = data)

##### Making interactive map
