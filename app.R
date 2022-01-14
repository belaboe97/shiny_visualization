# Load packages ----
library(shiny)
library(ggplot2)
library(car)
library(GGally)
#install.packages("janitor")
library(janitor)
library(dplyr)

# Source helper functions -----
source("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/helpers.R")

# Load data ----
df_music <-  read.csv("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/data/data.csv")
  #read.csv("data/data.csv")

# Clean and prep data ----
# TODO

clean<-clean_names(df_music)

#Get colnames
#https://dhruv-khurjekar.medium.com/investigating-spotifys-danceability-index-other-song-attributes-1983142f7dfd


#summary_df <- stat.desc(df_music) 
#print(summa

colnames(clean)

clean %>% 
  select(c("acousticness", "danceability", "duration_ms", "energy", "loudness", "tempo")) -> 
  df_song_data 

clean %>% 
  select(c("name", "artists", "popularity", "release_date", "year")) ->
  df_song_metadata


summary(df_song_data)
#ggpairs(df_song_data, aes(colour = "blue", alpha = 0.4))
# PLOT 1

# First conclusions: 
# all the values are numeric, 
# acousticness, danceability and energy might be scaled or artificially computed in an value range 
# between 0 and 1



summary(df_song_metadata)




print(clean_x)
# Make df reactive (faster loading when filtering on user inputs)
# TODO

years <- sort(unique(df_music$year))
# TODO Make dict and ensure that correct value is stored to its year + move to sever and make interactable
avg_per_year <- c()
for (i in seq(1,length(years), by=1)){
  avg_per_year[i] <- avg_maker('acousticness', 10, years[i])
}







avg_maker("acousticness")




# User interface ----
ui <- fluidPage(
  
  titlePanel("Music Explorer"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("var", 
                  label = "Select a variable to examine.",
                  choices = c("acousticness", "danceability",
                              "energy", "instrumentalness", 
                              "liveness", "loudness", "speechiness",
                              "tempo", "valence"),
                  selected = "acousticness"),
      textInput("top_num", "Top #", "10"),
      
      
      dateRangeInput("release_date",
                     "Date range",
                     start = "1921",
                     end = "2020"),
    ),
    
    mainPanel(plotOutput("plot"))
  )
)

# Server logic ----
server <- function(input, output) {
  
  dataInput <- reactive({  
    getSymbols(input$var, src = df_music,
               from = input$dates[1],
               to = input$dates[2],
               auto.assign = FALSE)
  })
  
  output$plot <- renderPlot({
    print(input$var)
    # TODO don't cheat on combination.. 
    plot.default(avg_maker(input$var))
  })
  
}

# Run app ----
shinyApp(ui, server)

