#main set of functions and libraries for project
library(tidyverse)
library(ExtDist)
library(ggpubr)
library(quarto)

#' @description Create "city" line list
#' @param n The number of cities to create (must be square term)
#' @param a The number of high and low pop cities 
#' @param avg The average size of cities 
#' @param scale How much larger / smaller cities will be than regular ones
create_cities <- function(n = 36, 
                          a = 3, 
                          avg = 900, 
                          scale = 2.5, 
                          seed = 1234){
  #forces cities to be square
  if(sqrt(n)%%1 != 0){
    print("The number of cities must be square")
    stop()
  }
  set.seed(seed)
  big_cities <- sample(1:n,a)
  small_cities <- big_cities
  while(sum(small_cities %in% big_cities)>0){
    small_cities <- sample(1:n,a)
  }
  
  set.seed(seed)
  cities <- tibble(
    "id" = 1:n,
    "pop" = rpois(n, avg), 
    "lon" = rep(1:sqrt(n), sqrt(n)), 
    "lat" = sapply(1:sqrt(n), function(x) rep(x, sqrt(n)), simplify = F) %>% unlist(), 
    "group" = c("A","A","B","B","B","C",
                "A","A","B","B","C","C", 
                "A","A","A","B","C","C",
                "A","A","E","E","E","C",
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
           id_y = id, lon_y = lon, lat_y = lat, group_y = group, trans) |>
    mutate(type = "Baseline")
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
    geom_text(aes(x = lon, y = lat, label = id), size = 3,nudge_y = -0.2) +
    theme_void() +
    #theme(legend.position = "None") +
    labs(size = "Population", color = "Group")
}

#' @description Plot transitions between cities
#' @param city_list Tibble of cities 
#' @param trans_list Tibble of transitions between cities
plot_trans <- function(city_list, trans_list, agg = F, noise = NULL){
  
  if(agg){
    y <- "Transition network between aggregated regions (darker means more transitions)"
  }else{
    y <- "Transition network between cities (darker means more transitions)"
  }
  
  if(!is.null(noise)){
    y <- paste0(y, ", Noise: ", noise)
  }
  
  
  p <- plot_cities(city_list) + 
    geom_segment(data = trans_list |> filter(trans > 0), 
                 aes(x = lon_x, y = lat_x, 
                     xend = lon_y, yend = lat_y, alpha = trans), size = 1) +
    labs(alpha = "# trans", title = y)
  
  if("Common" %in% trans_list$type %>% unique()){
    p <- p + 
      geom_segment(data = trans_list |> filter(type  == "Common"), 
                   aes(x = lon_x, y = lat_x, 
                       xend = lon_y, yend = lat_y), size = 1.5, color = "blue",
                   arrow = arrow(length = unit(0.03, "npc")))
  }
  
  if("Uncommon" %in% trans_list$type %>% unique()){
    p <- p + 
      geom_segment(data = trans_list |> filter(type  == "Uncommon"), 
                   aes(x = lon_x, y = lat_x, 
                       xend = lon_y, yend = lat_y), size = 1.5, color = "red",
                   arrow = arrow(length = unit(0.03, "npc")))
  }
  
  x <- paste0("Common transition (blue) - User is 1 out of ",
         filter(trans_list, type == "Common") |> pull(trans), 
         " transitions\n",
         "Unommon transition (red) - User is 1 out of ",
         filter(trans_list, type == "Uncommon") |> pull(trans), 
         " transitions")
  
  if("Common" %in% trans_list$type %>% unique() | 
     "Uncommon" %in% trans_list$type %>% unique()){
    p <- p + labs(subtitle = x)
  }
  
  
  return(p)
  
}

#' @description Create transitions list from prompt 
create_user_trans <- function(){
  print("Please choose two transitions that you would like to make!")
  print("The first should be one that seems common, so from a big city to another.")
  trans_1_1 <- readline(prompt="What is the starting location of your first transition: ") |> as.numeric()
  trans_1_2 <- readline(prompt="What is the ending location of your first transition: ") |> as.numeric()
  print("The second should be one that seems less common, perhaps even a transition that doesn't exist")
  trans_2_1 <- readline(prompt="What is the starting location of your second transition: ") |> as.numeric()
  trans_2_2 <- readline(prompt="What is the ending location of your second transition: ") |> as.numeric()
  list(
    "Common" = list("start" = trans_1_1, "end" = trans_1_2),
    "Uncommon" = list("start" = trans_2_1, "end" = trans_2_2)
  )
}

#' @description Aggregate city and transition data
#' @param city_list Tibble of city info 
#' @param trans_list Tibble of trans info
aggregate_network <- function(city_list, trans_list){
  agg_city_list <- city_list |>
    group_by(group) |>
    summarise(id = group,
              pop = sum(pop),
              lon = mean(lon),
              lat = mean(lat)) |>
    ungroup()
  
  agg_trans_list <- trans_list |>
    group_by(group_x, group_y) |>
    summarise(lon_x = mean(lon_x),
              lat_x = mean(lat_x),
              lon_y = mean(lon_y),
              lat_y = mean(lat_y),
              trans = sum(trans)) |>
    ungroup() |>
    mutate(type = "Baseline")
  
  if("Common" %in% trans_list$type){
    x <- trans_list |>
      filter(type == "Common") |>
      select(group_x, group_y, type)
    
    agg_trans_list <- agg_trans_list %>%
      mutate(type = ifelse(group_x == x$group_x & group_y == x$group_y, "Common", type))
    
  }
  
  if("Uncommon" %in% trans_list$type){
    x <- trans_list |>
      filter(type == "Uncommon") |>
      select(group_x, group_y, type)
    
    agg_trans_list <- agg_trans_list %>%
      mutate(type = ifelse(group_x == x$group_x & group_y == x$group_y, "Uncommon", type))
    
  }
  
  return(list("agg_city" = agg_city_list, "agg_trans" = agg_trans_list))
  
}

#' @description Choose and update transition lists (change trans )
#' @param trans_list Tibble of transitions
#' @param chosen_trans Named list of transitions
add_trans <- function(trans_list, chosen_trans){
  trans_list |>
    mutate(trans = ifelse(id_x == chosen_trans$Common$start & id_y == chosen_trans$Common$end, trans + 1, trans), 
           trans = ifelse(id_x == chosen_trans$Uncommon$start & id_y == chosen_trans$Uncommon$end, trans + 1, trans), 
           type = case_when(
             id_x == chosen_trans$Common$start & id_y == chosen_trans$Common$end ~ "Common",
             id_x == chosen_trans$Uncommon$start & id_y == chosen_trans$Uncommon$end ~ "Uncommon", 
             T ~ "Baseline"
           )) 
}

#' @description Add noise to transitions 
#' @param trans_list Tibble of transitions
#' @param noise The amoung of noise to add to transitions
add_noise <- function(trans_list, noise = 0.5){
  trans_list$diff <- rLaplace(n = nrow(trans_list), mu = 0, b = noise)
  trans_list <- trans_list |> 
    mutate(trans = round(trans + diff, 0), 
           trans = ifelse(trans < 0, 0, trans)) |>
    select(-diff)
}
