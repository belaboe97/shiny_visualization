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
library(htmltools)
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

filterList = list(
                  "acousticness" = T, 
                  "danceability" = T, 
                  "duration_ms" = T, 
                  "energy" = T, 
                  "loudness" = T, 
                  "tempo" = T)

get_properties = function (flsit){return(names(filterList[filterList==T]))}

get_data_mean = function(data,years){
  clean %>% 
    select(get_properties(data), year) %>% 
    slice_rows("year") %>% 
    dmap(mean) %>% 
    filter(year > years[1] & year < years[2])-> 
    df_song_data
  df_song_data =  melt(df_song_data, id="year")
  return(df_song_data)
}

get_data_median = function(data){
  clean %>% 
    select(get_properties(mean_val_song_data),year) %>% 
    group_by("year") %>% 
     summarise(across(everything(), list(median))) ->
     median_val_song_data
     colnames(median_val_song_data) = colnames(df_song_data)
  #df_song_data =  melt(median_val_song_data, id="year")
  return(df_song_data)
}


clean %>% select(duration_ms, loudness, tempo) %>% 
  lapply( function(x){(x-min(x))/(max(x)-min(x))}) -> 
  scaled_values

clean$tempo = scaled_values$tempo
clean$loudness = scaled_values$loudness
clean$duration_ms = scaled_values$duration_ms


clean %>% 
  select(c("name", "artists", "popularity", "release_date", "year")) ->
  df_song_metadata

df_song_data_mean = get_data_mean(clean,c("1921","2020"))
df_song_data_median = get_data_median(clean)


clean %>% 
  select(c("year", "acousticness", "danceability", "duration_ms", "energy", "loudness", "tempo")) -> 
  df_song_data 

df_song_data %>% 
  slice_rows("year") %>% 
  dmap(mean)  -> 
  mean_val_song_data

mean_val_song_data <- melt(mean_val_song_data, id="year")

# df_song_data %>% 
#   slice_rows("year") %>% 
#   dmap(mean)  -> 
#   mean_val_song_data



# df_song_data %>% 
#  group_by(year) %>% 
#  summarise(across(everything(), list(median))) ->
#  median_val_song_data
#  colnames(median_val_song_data) = colnames(df_song_data)


summary(df_song_data_mean)
#summary(df_song_data_median)
# First conclusions: 
# all the values are numeric, 
# acousticness, danceability and energy might be scaled or artificially computed in an value range 
# between 0 and 1
summary(df_song_metadata)


# Make df reactive (faster loading when filtering on user inputs)
# TODO




# User interface ----
ui <- fluidPage(
  #theme = shinytheme("united"),
  
  tags$head(
    tags$style(type="text/css", ".irs { width: 100% }"),
    tags$style(HTML('#btn_mean{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_median{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_boxplot{margin:1rem; width:100%}')),
    tags$style(HTML('.btn-buttom {width: 15%}')),
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
                                  class="btn btn-primary",
                                  style="margin:1rem; width:100%", 
                                  ),

                    bsButton("btn_median", "median", 
                                 icon("equals"), 
                                 class="btn",
                                 style="margin:1rem; width:100%",
                                 ),
                    
                    bsButton("btn_boxplot", "boxplot", 
                                 icon("box-open"), 
                                 class="btn-buttom",
                                 style="margin:1rem; width:100%", 
                    ),
            ))),
    column(10, style="align-items:center",
           plotOutput("avg_per_year_plot")),
   
    column(2),
    column(10,style="margin-top:0.5rem,left:3rem",
             actionButton("acoust", "acoustics", 
                          icon("guitar"),
                          style="background-color:#F2755B",
                          class="btn-buttom"
                          ),
           
             actionButton("danceab", "dancy", 
                          icon("drum-steelpan"), 
                          style="background-color:#F2A65B",
                          class="btn-buttom"
                          ),
           
             actionButton("dura", "duration", 
                          icon("hourglass-half"), 
                          style="background-color:#F2DD5B",
                          class="btn-buttom"
                          ),
           
             actionButton("engy", "energy", 
                          icon("bolt"), 
                          style="background-color:#BEF25B",
                          class="btn-buttom"
                          ),
             actionButton("loud", "loudness", 
                        icon("volume-up"), 
                        style="background-color:#60F25B",
                        class="btn-buttom"
                        ),
             actionButton("tempo", "tempo", 
                        icon("tachometer-alt"), 
                        style="background-color:#5BF2DA",
                        class="btn-buttom"
                        ),
    )

  )

)


d1 = get_data_median(clean)

d2 = get_data_mean(clean,c("1921","2020"))

length(get_properties(clean))

switch_btn = function (value_string){

  if(length(get_properties(clean)) > 1 || filterList[[value_string]] == F ){
    if(filterList[[value_string]]) filterList[value_string]<<- F else filterList[value_string] <<- T 
    print(filterList[[value_string]])
    }
  else{
    showNotification("Atleast one graph must be displayed", type="error")
  }
}
#print(d1)
# Server logic ----
server <- function(input, output,session) {
  
  year_avg_year = reactive({ input$year_avg_year })
  reactv = reactiveValues(plot_data = get_data_mean(clean,c("1921","2020")) , color="red")
  
  observeEvent(input$year_avg_year,{
    reactv$plot_data <- get_data_mean(clean,input$year_avg_year)
  })

  observeEvent(input$btn_mean, {
    reactv$plot_data = mean_val_song_data
    reactv$color="red"
    updateButton(session,'btn_median',style = "secondary")
    updateButton(session,'btn_boxplot',style = "secondary")
    updateButton(session,'btn_mean',style = "primary")
  })
  
  observeEvent(input$btn_median, {
    data$plot_data = d1
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
  
  
  observeEvent(input$acoust, {
    switch_btn("acousticness")          
    reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
  observeEvent(input$danceab, {
    switch_btn("danceability")   
    reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
  observeEvent(input$dura, {
    switch_btn("duration_ms")   
    reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
  observeEvent(input$engy, {
   switch_btn("energy")   
   reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
  observeEvent(input$loud, {
   switch_btn("loudness") 
   reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
  observeEvent(input$tempo, {
   switch_btn("tempo") 
   reactv$plot_data <- get_data_mean(clean,year_avg_year())})
  
   output$avg_per_year_plot = renderPlot({ 

   ggplot(
      reactv$plot_data,aes(x=year,y=value, colour=variable))+
      geom_point(shape=18)+
       scale_color_manual(name = "Properties", 
                          values = c("acousticness" = "#F2755B", 
                                     "danceability" = "#F2A65B", 
                                     "duration_ms" = "#F2DD5B",
                                     "energy" = "#BEF25B", 
                                     "loudness" = "#60F25B", 
                                     "tempo" = "#5BF2DA"))+
      geom_smooth()
  })
}



  
  
# Run app ----
shinyApp(ui, server)

