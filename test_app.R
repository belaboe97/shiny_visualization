# Load packages ----
library(shiny)
library(shinythemes)
library(ggplot2)
library(car)
library(GGally)
library(stringr)
library(plotly)
library(DT)
library(purrrlyr)
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
  select(c("year", "acousticness", "danceability", "duration_ms", "energy", "loudness", "tempo")) -> 
  df_song_data 

clean %>% 
  select(c("name", "artists", "popularity", "release_date", "year")) ->
  df_song_metadata


df_song_data %>% 
  slice_rows("year") %>% 
  dmap(mean) -> 
  mean_val_song_data

df_song_data %>% 
 group_by(year) %>% 
 summarise(across(everything(), list(median))) ->
 median_val_song_data
 colnames(median_val_song_data) = colnames(df_song_data)


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
                     actionButton("btn_mean", "average", 
                                  icon("align-center"), 
                                  class="btn btn-primary mt-5",
                                  style="margin:1rem; width:100%", 
                                  ),

                    actionButton("btn_median", "median", 
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


plot_data = mean_val_song_data 

# Server logic ----
server <- function(input, output) {
  
  btn_mean = reactive({ input$btn_mean }) 
  
  btn_median = reactive({ input$btn_median }) 
  
  observeEvent(input$btn_mean, {
    plot_data = mean_val_song_data
  })
  
  observeEvent(input$btn_median, {
    plot_data = median_val_song_data
  })  
  
  
  output$avg_per_year_plot = renderPlot({ 

   ggplot(
      plot_data,aes(x=year,y=acousticness))+
      geom_point(shape=18, color="blue")+
      geom_smooth()
  })
}





  

  
  
# Run app ----
shinyApp(ui, server)

