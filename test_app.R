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
  theme = shinytheme("united"),
  
  tags$head(
    tags$style(type="text/css", ".irs { width: 100% }")
  ),

   fluidRow(
    column(2,), 
    column(10,style="left:3rem;",
           sliderInput("year_avg_year",
                       label = "Years", min = 1921, 
                       max = 2021, 
                       value = c(1921, 2021)),     
           ),
     
    column(2,
            fluidRow(
              column(12,style="",
                     actionButton("runif", "average", 
                                  icon("align-center"), 
                                  class="btn btn-primary mt-5",
                                  style="margin:1rem; width:100%", 
                                  ),

                    actionButton("runif", "median", 
                                 icon("equals"), 
                                 class="btn",
                                 style="margin:1rem; width:100%", 
                                 ),
                    
                    actionButton("runif", "boxplot", 
                                 icon("box-open"), 
                                 class="btn",
                                 style="margin:1rem; width:100%", 
                    ),
            ))),
    column(10, style="align-items:center",
           plotOutput("avg_per_year_plot")),
   
    column(2),
    column(10,style="margin-top:0.5rem,left:3rem",
             actionButton("runif", "acousticness", 
                          icon("guitar"), 
                          class="btn btn-success"),
           
             actionButton("runif", "danceability", 
                          icon("volume-up"), 
                          class="btn btn-danger"
                          ),
           
             actionButton("runif", "duration", 
                          icon("hourglass-half"), 
                          class="btn btn-warning"),
           
             actionButton("runif", "energy", 
                          icon("bolt"), 
                          class="btn btn-info"),
           
    )
  
  )

)

print(      avg_maker(clean, 
                      "danceability", 
                      c(1921, 2021)))


# Server logic ----
server <- function(input, output) {

  

  
  
  output$avg_per_year_plot = renderPlot({ 
    
   ggplot(
      avg_maker(clean,
                "danceability", 
                c(1921, 2021)),aes(x=year,y=mean_values))+
      geom_point(shape=18, color="blue")+
      geom_smooth()
  })

}





# Run app ----
shinyApp(ui, server)

