#main set of functions and libraries for project
library(tidyverse)

#' @description Create "city" line list
#' @param n The number of cities to create (must be square term)
#' @param a The number of high and low pop cities 
#' @param avg The average size of cities 
#' @param scale How much larger / smaller cities will be than regular ones
create_cities <- function(n = 36, 
                          a = 3, 
                          avg = 900, 
                          scale = 2.5){
  #forces cities to be square
  if(sqrt(n)%%1 != 0){
    print("The number of cities must be square")
    stop()
  }
  
  big_cities <- sample(1:n,a)
  small_cities <- big_cities
  while(sum(small_cities %in% big_cities)>0){
    small_cities <- sample(1:n,a)
  }
  
  
  cities <- tibble(
    "id" = 1:n,
    "pop" = rpois(n, avg), 
    "lon" = rep(1:sqrt(n), sqrt(n)), 
    "lat" = sapply(1:sqrt(n), function(x) rep(x, sqrt(n)), simplify = F) %>% unlist(), 
    "group" = c("A","A","B","B","B","C",
                "A","D","B","B","C","C", 
                "D","D","D","B","C","C",
                "D","D","E","E","E","C",
                "F","F","F","E","E","G",
                "F","F","E","G","G","G")
  ) |>
    mutate(pop = ifelse(id %in% big_cities, pop*scale, pop),
           pop = ifelse(id %in% small_cities, pop*(1/scale), pop))
  
  return(cities)
  
}

#' @description Create transitions between all cities
#' @param city_list Tibble of list of cities preformatted
create_trans <- function(index, city_list){
  index_id <- city_list[index,] |> pull(id)
  index_lat <- city_list[index,] |> pull(lat)
  index_lon <- city_list[index,] |> pull(lon)
  index_pop <- city_list[index,] |> pull(pop)
  index_group <- city_list[index,] |> pull(group)
  
  city_list[-index,] |>
    mutate(dist = sqrt((lat-index_lat)^2 + (lon-index_lon)^2)*100, 
           trans = round((pop*index_pop)/dist^2.8,0), 
           id_x = index_id, 
           lon_x = index_lon, 
           lat_x = index_lat, 
           group_x = index_group) |>
    select(id_x, lon_x, lat_x, group_x,
           id_y = id, lon_y = lon, lat_y = lat, group_y = group, trans)
}

#' @description Create all transitions
#' @param city_list Tibble of all cities preformatted
create_all_trans <- function(city_list){
  lapply(1:nrow(city_list), function(x) create_trans(x, city_list)) |>
    bind_rows()
}

#' @description Plot cities based on cities input
#' @param city_list Tibble of list of cities preformatted
plot_cities <- function(city_list){
  ggplot(data = city_list) +
    geom_point(aes(x = lon, y = lat, size = pop, color = group)) +
    geom_text(aes(x = lon, y = lat, label = id), size = 3,nudge_y = -0.3) +
    theme_void() +
    theme(legend.position = "None") +
    labs(size = "Population", color = "Group")
}

#' @description Plot transitions between cities
#' @param city_list Tibble of cities 
#' @param trans_list Tibble of transitions between cities
plot_trans <- function(city_list, trans_list){
  plot_cities(city_list) + 
    geom_segment(data = trans_list |> filter(trans > 0), 
                 aes(x = lon_x, y = lat_x, 
                     xend = lon_y, yend = lat_y,
                     alpha = trans), size = 0.5)
}
