# Load packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(car)
library(GGally)
library(stringr)
library(plotly)
library(DT)
#install.packages("janitor")
library(janitor)
library(dplyr)

# Source helper functions -----
source("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/helpers.R")
source("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/styles.R")
# Load data ----
df_music =  read.csv("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/data/data.csv")
#read.csv("data/data.csv")

# Clean and prep data ----
# TODO

clean<-clean_names(df_music)



#summary_df <- stat.desc(df_music) 


colnames(clean)

#Get colnames
#https://dhruv-khurjekar.medium.com/investigating-spotifys-danceability-index-other-song-attributes-1983142f7dfd


clean %>% 
  select(c("acousticness", "danceability", "duration_ms", "energy", "loudness", "tempo")) -> 
  df_song_data 

clean %>% 
  select(c("name", "artists", "popularity", "release_date", "year")) ->
  df_song_metadata




summary(df_song_data)
# First conclusions: 
# all the values are numeric, 
# acousticness, danceability and energy might be scaled or artificially computed in an value range 
# between 0 and 1
summary(df_song_metadata)


# Make df reactive (faster loading when filtering on user inputs)
# TODO




# User interface ----
ui <- fluidPage(
  theme = shinytheme("spacelab"),
  
  tags$head(
    tags$style(HTML('#run{background-color:orange}'))
  ),

   fluidRow(
     
    column(2,
           lapply(1:5, function(iter) {
             actionButton("runif", paste0("variable",iter), class="btn-warning")
           })),
    column(10,
           plotOutput("avg_per_year_plot")),
   
    column(2),
    column(10,
           lapply(1:5, function(iter) {
             actionButton("runif", paste0("variable",iter), 
                          icon("paper-plane"), 
                          style="color: blue; background-color: white; border-color: #2e6da4")
           })
    )
  
  )

)

# Server logic ----
server <- function(input, output) {

  

  
  
  output$avg_per_year_plot = renderPlot({ 
    
    plot.default(
      avg_maker(clean, 
                "danceability", 
                c(1921, 2021)))
  })

}



# Run app ----
shinyApp(ui, server)

