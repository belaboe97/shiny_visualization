# Helper functions for music explorer


#Create Average for each year
avg_maker = function(song_data, category, years){
  category = rlang::sym(category)

  song_data %>% 
    group_by(year) %>% 
    summarise(mean_values = mean(!!category)) %>%
    mean_per_year
  
  return(mean_per_year)
}

