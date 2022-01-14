# Helper functions for music explorer


#Create Average for each year
avg_maker = function(category){
  category <- rlang::sym(category)
  clean %>% group_by(year) %>% summarise(mean_values = mean(!!category)) -> mean_per_year
  print(mean_per_year)
  return(mean_per_year)
}
