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
library(shinyBS)
library("reshape2")

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


clean$tempo = scaled_values$tempo
clean$loudness = scaled_values$loudness
clean$duration_ms = scaled_values$duration_ms

clean %>% 
  select(c("year", "acousticness", "danceability", "duration_ms", "energy", "loudness", "tempo")) -> 
  df_song_data 


clean %>% 
  select(c("name", "artists", "popularity", "release_date", "year")) ->
  df_song_metadata

clean %>% select(duration_ms, loudness, tempo) %>% 
  lapply( function(x){(x-min(x))/(max(x)-min(x))}) -> 
  scaled_values




df_song_data %>% 
  slice_rows("year") %>% 
  dmap(mean)  -> 
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
    tags$style(type="text/css", ".irs { width: 100% }"),
    tags$style(HTML('#btn_mean{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_median{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_boxplot{margin:1rem; width:100%}')),
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
                     bsButton("btn_mean", "average", 
                                  icon("align-center"), 
                                  class="sideBtns btn btn-primary",
                                  style="margin:1rem; width:100%", 
                                  ),

                    bsButton("btn_median", "median", 
                                 icon("equals"), 
                                 class="btn",
                                 style="margin:1rem; width:100%",
                                 ),
                    
                    bsButton("btn_boxplot", "boxplot", 
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



mean_val_song_data <- melt(mean_val_song_data, id="year")

# Server logic ----
server <- function(input, output,session) {
  
  reactv = reactiveValues(plot_data = mean_val_song_data, color="red")
  
  year_avg_year = reactive({ input$year_avg_year })
  
  observeEvent(input$btn_mean, {
    reactv$plot_data = mean_val_song_data
    reactv$color="red"
    updateButton(session,'btn_median',style = "secondary")
    updateButton(session,'btn_boxplot',style = "secondary")
    updateButton(session,'btn_mean',style = "primary")
  })
  
  observeEvent(input$btn_median, {
    data$plot_data = median_val_song_data
    reactv$color="blue"
    updateButton(session,'btn_median',style = "primary")
    updateButton(session,'btn_boxplot',style = "secondary")
    updateButton(session,'btn_mean',style = "secondary")
  })  
  
  observeEvent(input$btn_boxplot, {
    data$plot_data = median_val_song_data
    reactv$color="blue"
    updateButton(session,'btn_median',style = "secondary")
    updateButton(session,'btn_boxplot',style = "primary")
    updateButton(session,'btn_mean',style = "secondary")
  })  

  
   output$avg_per_year_plot = renderPlot({ 
    
   print(reactv$color)

   ggplot(
      reactv$plot_data,aes(x=year,y=value, colour=variable))+
      geom_point(shape=18, color=reactv$color)+
      geom_smooth()
  })
}

  
  
# Run app ----
shinyApp(ui, server)

