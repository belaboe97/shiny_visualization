# Helper functions for music explorer

# TODO add top # parameter and standardize length
avg_maker <- function(var, top_num, year_g){
  df_music_year <- df_music[df_music$year==as.character(year_g),]
  avg_var <- mean(df_music_year[, c(var)])
  return(avg_var)
}