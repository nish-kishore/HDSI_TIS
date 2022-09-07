library(here)
source(here("code","src.R"))

#create list of cities 
city_list <- create_cities()

#visualize cities 
plot_cities(city_list)

#create baseline transitions between cities
trans_list <- create_all_trans(city_list)

#visualize transitions 
plot_trans(city_list, trans_list)

#generate user transitions
chosen_trans <- create_user_trans()

#add user transitions
trans_list_user <- add_trans(trans_list, chosen_trans)

#visualize user transitions
plot_trans(city_list, trans_list_user)

#aggregate data
x <- aggregate_network(city_list, trans_list_user)

#visualize aggregated data
plot_trans(x$agg_city, x$agg_trans, agg = T)

#visualize with small amounts of noise
noisy_trans <- add_noise(trans_list_user, noise = 0.2)
a <- plot_trans(city_list, noisy_trans, noise = 0.2)
x <- aggregate_network(city_list, noisy_trans)
b <- plot_trans(x$agg_city, x$agg_trans, agg = T)
ggarrange(a,b, common.legend = T, ncol = 1, legend = "right")

#visualize with a bit more noise
noisy_trans <- add_noise(trans_list_user, noise = 0.5)
a <- plot_trans(city_list, noisy_trans, noise = 0.5)
x <- aggregate_network(city_list, noisy_trans)
b <- plot_trans(x$agg_city, x$agg_trans, agg = T)
ggarrange(a,b, common.legend = T, ncol = 1, legend = "right")

#visualize with more noise
noisy_trans <- add_noise(trans_list_user, noise = 1)
a <- plot_trans(city_list, noisy_trans, noise = 1)
x <- aggregate_network(city_list, noisy_trans)
b <- plot_trans(x$agg_city, x$agg_trans, agg = T)
ggarrange(a,b, common.legend = T, ncol = 1, legend = "right")
