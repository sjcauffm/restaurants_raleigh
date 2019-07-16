library(tidyverse)
library(rvest)
library(selectr)
library(xml2)
library(dplyr)
library(tidyr)
library(plyr)
library(purrr)


#Loading data 
google_url <- html_session("https://www.tripadvisor.com/Restaurants-g49463-Raleigh_North_Carolina.html")

#####Getting restaurant data
get_restaurant<- function(restaurant) {
name <- restaurant %>%
  html_nodes(".h1") %>%
  html_text()

rating <- restaurant %>%
  html_nodes(".restaurants-detail-overview-cards-RatingsOverviewCard__overallRating--nohTl") %>%
  html_text()
rating <- str_trim(rating, "right")

review_total <- restaurant %>%
  html_nodes(".reviews_header_count") %>%
  html_text()

review_total_split <- gsub("\\(", "", review_total)
review_total_split <- gsub("\\)", "", review_total_split)
review_total_num <- review_total_split

address <- restaurant %>%
  html_nodes(".street-address") %>%
  html_text()

locality <- restaurant %>%
  html_nodes(".locality") %>%
  html_text()

phone <- restaurant %>%
  html_nodes(".detail.is-hidden-mobile") %>%
  html_text

phone_split <- str_split(phone, " ")
phone_split_unlist <- unlist(phone_split)
phone_num <- phone_split_unlist[2]

price_import <- restaurant %>%
  html_nodes(".header_links a:nth-child(1)") %>%
  html_text

cuisine_tags <- restaurant %>%
  html_nodes("#taplc_resp_rr_top_info_rr_resp_0 a+ a") %>%
  html_text()

restaurant_data <- data.frame(name = name,
                              rating = rating,
                              reviews = review_total_num,
                              address = address,
                              locality = locality,
                              phone_number = phone_num,
                              price = price_import,
                              cusine = unlist(cuisine_tags))
restaurant_data
}



##Looping to get the list of all restaurants from the first page
get_restaurants <- function (restaurant_URLs){
 data <- data.frame()
 i = 1
 for(i in 1:length(restaurant_URLs)){
   tripadvisor_restaurant <- read_html(paste0("https://www.tripadvisor.com", restaurant_URLs[i]))
   restaurant_data <- get_restaurant(tripadvisor_restaurant)
   data <- rbind.fill(data, restaurant_data)
   print(i)
 }
 data
}

#### Another Loop to get the rest of the restaurants
## get page urls

jump <- seq(30, 150, by = 30)
site <- paste("https://www.tripadvisor.com/Restaurants-g49463-oa", jump, 
              "-Raleigh_North_Carolina.html#EATERY_LIST_CONTENTS", sep = "")

test <- read_html(site[1])

urls <- lapply(site, function(i){
  temp <- read_html(i)
  
  url_temp <- temp %>%
    html_nodes(".property_title") %>%
    html_attrs()
})

url_vect <- as.data.frame(unlist(urls))
test <- grep("g49463",url_vect$`unlist(urls)`)
url_vect_2 <- url_vect[test,]
url_vect_2 <- as.character(url_vect_2)
test_2 <- grep("ta.restaurant_list_tracking.clickDetailTitle", url_vect_2)
url_vect_3 <- url_vect_2[-test_2]




raleigh_rest_1 <- get_restaurants(url_vect_3[c(1:7, 9:50)])
raleigh_rest_2 <- get_restaurants(url_vect_3[c(51:94)])
raleigh_rest_3 <- get_restaurants(url_vect_3[c(95:96, 98:120)])

broken <- get_restaurants(url_vect_3[8])
broken_2 <- get_restaurants(url_vect_3[97])

raleigh_rest <- rbind(raleigh_rest_1, raleigh_rest_2, raleigh_rest_3)

write.csv(raleigh_rest, "raleigh_restaurants.csv")


### there are issues with restaurants 8, 97...






























