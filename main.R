#Set up the simulation
city_list <- create_cities()
trans_list <- create_all_trans()

#show user the transition figure
plot_trans(city_list, trans_list)

#walk them through the figure and ask them to choose
#a common transition and an uncommon transition
chosen_trans <- create_user_trans()

#render the following presentation and walk them through the results
quarto_render(input = here("code","pres.qmd"),
              execute_params = list("common_start"=chosen_trans$Common$start,
                                    "common_end"=chosen_trans$Common$end,
                                    "uncommon_start"=chosen_trans$Uncommon$start,
                                    "uncommon_end"=chosen_trans$Uncommon$end))
