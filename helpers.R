# Helper functions for music explorer


#Create Average for each year
avg_maker = function(song_data, category, years){
    category = rlang::sym(category)
    start_year = years[1]
    end_year = years[2]
    
    song_data %>% 
    group_by(year) %>% 
    summarise(mean_values = mean(!!category)) %>%
    filter(year > start_year & year < end_year)-> 
    mean_per_year
    
  return(mean_per_year)
}


most_popular_songs = function(data, wanted_year) {

  data %>% arrange(across(starts_with("popularity"), desc)) %>% 
    filter( year == as.numeric(wanted_year))  %>% 
    data.frame() ->
    most_popular_artists
  res = head(most_popular_artists,5)
  return(res)
}

search_artist_mps = function(data, wanted_artist,wanted_year){
  
  data %>% arrange(across(starts_with("popularity"), desc)) %>% 
    filter( year == as.numeric(wanted_year)) %>% 
    filter(grepl(wanted_artist, artists, fixed = TRUE)) %>% 
    data.frame() -> 
    searched_artist
  res = head(searched_artist,1)
  return(res)
}

pretty_radarchart <- function(data, color = "#00AFBB", 
                              vlabels = colnames(data), vlcex = 0.7,
                              caxislabels = NULL, title = NULL){
  radarchart(
    data, axistype = 1,
    # Customize the polygon
    pcol = color, pfcol = scales::alpha(color, 0.5), plwd = 2, plty = 1,
    # Customize the grid
    cglcol = "grey", cglty = 1, cglwd = 0.8,
    # Customize the axis
    axislabcol = "grey", 
    # Variable labels
    vlcex = vlcex, vlabels = vlabels,
    caxislabels = caxislabels, title = title,
  )
}
