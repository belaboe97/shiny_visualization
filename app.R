# install packages ----
# install.packages("shiny")
# install.packages(shinythemes)
# install.packages(ggplot2)
# install.packages(car)
# install.packages(GGally)
# install.packages(stringr)
# install.packages(plotly)
# install.packages(DT)
# install.packages(janitor)
# install.packages(dplyr)
# install.packages(ggcorrplot)
# install.packages(corrplot)
# install.packages(rlang)
# install.packages(tidyverse)
# install.packages(hrbrthemes)
# install.packages(fmsb)
# install.packages(shinyBS)
# install.packages("reshape2")
# install.packages(janitor)
# install.packages(dplyr)
# install.packages(purrrlyr)

#load packages

library(shiny)
library(shinythemes)
library(ggplot2)
library(car)
library(GGally)
library(stringr)
library(plotly)
library(DT)
library(janitor)
library(dplyr)
library(ggcorrplot)
library(corrplot)
library(rlang)
library(tidyverse)
library(hrbrthemes)
library(fmsb)
library(shinyBS)
library("reshape2")
library(janitor)
library(dplyr)
library(purrrlyr)

#deployment
rsconnect::setAccountInfo(name='1shinyvismadrid',
                          token='25F28D8B9547AC326841C54657FA09DA',
                          secret='UzoCgSeQqQgJ/T5SCyrdXoROFXJwlJul6x3fGJOy')

# Source helper functions -----

#source("helper.R")
#source("ui.R")
#source("server.R")


# Helper functions for music explorer

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

get_data_median = function(data,year){
  clean %>% 
    select(get_properties(mean_val_song_data),year) %>% 
    group_by("year") %>% 
    summarise(across(everything(), list(median))) ->
    median_val_song_data
  colnames(median_val_song_data) = colnames(df_song_data)
  df_song_data =  melt(median_val_song_data, id="year")
  return(df_song_data)
}

get_data_boxplot = function(data,years){
  boxplot_data = clean %>% 
    select(get_properties(clean), year) %>% melt(id.vars="year") %>%
    filter(year > years[1] & year < years[2])
  return(boxplot_data)
}


switch_btn = function (value_string){
  
  if(length(get_properties(clean)) > 1 || filterList[[value_string]] == F ){
    if(filterList[[value_string]]) filterList[value_string]<<- F else filterList[value_string] <<- T 
    print(filterList[[value_string]])
  }
  else{
    showNotification("Atleast one graph must be displayed", type="error")
  }
}

reload_data = function(data, year, choice){
  res = c()
  if(choice == 0){
    res = get_data_mean(clean,year)
  }
  else if(choice ==1){
    res = get_data_median(clean,year)
  }
  else if(choice ==2){
    res = get_data_boxplot(clean,year)
  }
  return(res)
}


filterList = list(
  "acousticness" = T, 
  "danceability" = T, 
  "duration_ms" = T, 
  "energy" = T, 
  "loudness" = T, 
  "tempo" = T)

color_coding = c("acousticness" = "#F2755B", 
                 "danceability" = "#F2A65B", 
                 "duration_ms" = "#F2DD5B",
                 "energy" = "#BEF25B", 
                 "loudness" = "#60F25B", 
                 "tempo" = "#5BF2DA")




# Load data ----
df_music = read.csv("data/data.csv")

# Clean and prep data ----
# TODO

clean<-clean_names(df_music)
clean$duration_s<- clean$duration_ms/1000

#summary_df <- stat.desc(df_music) 

colnames(clean)


clean %>% select(duration_ms, loudness, tempo) %>% 
  lapply( function(x){(x-min(x))/(max(x)-min(x))}) -> 
  scaled_values

clean$tempo = scaled_values$tempo
clean$loudness = scaled_values$loudness
clean$duration_ms = scaled_values$duration_ms

#Get colnames
#https://dhruv-khurjekar.medium.com/investigating-spotifys-danceability-index-other-song-attributes-1983142f7dfd

clean %>% 
  select(c("name", "artists", "popularity", "year","acousticness", "danceability", "duration_s", "energy", "explicit", "instrumentalness", "key", "liveness", 
           "loudness", "mode" , "speechiness", "tempo", "valence")) ->
  df_search_table_Data

clean %>% 
  select(c("acousticness", "danceability", "duration_s", "energy", "explicit", "instrumentalness", "key", "liveness", 
           "loudness", "mode" , "speechiness", "tempo", "valence", "year")) -> 
  detail_year_song_data 


ui <- fluidPage(
  #theme = shinytheme("spacelab"),
  
  tags$head(
    tags$style(type="text/css", ".irs { width: 100% }"),
    tags$style(HTML('#btn_mean{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_median{margin:1rem; width:100%}')),
    tags$style(HTML('#btn_boxplot{margin:1rem; width:100%}')),
    tags$style(HTML('.btn-buttom {width: 15%}')),
  ),
  
  titlePanel("Music Explorer"),
  
  tabsetPanel(
    tabPanel("Introduction", fluid = TRUE,
             
             br(),
             tags$b(h1("Definition of Features")),
             h3("Acousticness"),
             h5("A confidence measure from 0.0 to 1.0 of whether the track is acoustic. 1.0 represents high confidence the track is acoustic."),
             h3("Danceability"),
             h5("Danceability describes how suitable a track is for dancing based on a combination of musical elements including tempo, rhythm stability, beat strength, and overall regularity. A value of 0.0 is least danceable and 1.0 is most danceable."),
             h3("Duration_s"),
             h5("The duration of the track in seconds."),
             h3("Energy"),
             h5("Energy is a measure from 0.0 to 1.0 and represents a perceptual measure of intensity and activity. Typically, energetic tracks feel fast, loud, and noisy"),
             h3("Explicit"),
             h5("A binary value where 0 represents an non-explicit song and 1 is an explicit song."),
             h3("Instrumentalness"),
             h5("Predicts whether a track contains no vocals. \"Ooh\" and \"aah\" sounds are treated as instrumental in this context. Rap or spoken word tracks are clearly \"vocal\". The closer the instrumentalness value is to 1.0, the greater likelihood the track contains no vocal content. Values above 0.5 are intended to represent instrumental tracks, but confidence is higher as the value approaches 1.0."),
             h3("Key"),
             h5("The key the track is in. Integers map to pitches using standard Pitch Class notation. E.g. 0 = C, 1 = C♯/D♭, 2 = D, and so on. If no key was detected, the value is -1."),
             h3("Liveness"),
             h5("Detects the presence of an audience in the recording. Higher liveness values represent an increased probability that the track was performed live. A value above 0.8 provides strong likelihood that the track is live."),
             h3("Loudness"),
             h5("The overall loudness of a track in decibels (dB). Loudness values are averaged across the entire track and are useful for comparing relative loudness of tracks."),
             h3("Mode"),
             h5("Mode indicates the modality (major or minor) of a track, the type of scale from which its melodic content is derived. Major is represented by 1 and minor is 0."),
             h3("Speechiness"),
             h5("Speechiness detects the presence of spoken words in a track. The more exclusively speech-like the recording (e.g. talk show, audio book, poetry), the closer to 1.0 the attribute value. Values above 0.66 describe tracks that are probably made entirely of spoken words. Values between 0.33 and 0.66 describe tracks that may contain both music and speech, either in sections or layered, including such cases as rap music. Values below 0.33 most likely represent music and other non-speech-like tracks."),
             h3("Tempo"),
             h5("The overall estimated tempo of a track in beats per minute (BPM). In musical terminology, tempo is the speed or pace of a given piece and derives directly from the average beat duration."),
             h3("Valence"),
             h5("A measure from 0.0 to 1.0 describing the musical positiveness conveyed by a track. Tracks with high valence sound more positive (e.g. happy, cheerful, euphoric), while tracks with low valence sound more negative (e.g. sad, depressed, angry)."),
    ),
    tabPanel("General Informations", fluid = TRUE,
             
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
    ),
    
    tabPanel("Search", fluid = TRUE,
             fluidRow(
               column(12,
                      h3("Search Songs about your favorite artists"),
                      strong("Todo:"),
                      code("Utf-8 encoding of the variables, prep"),
                      br(),
                      hr(),
                      # DT::dataTableOutput("stats_about_artist_table")
                      DTOutput("stats_about_artist_table")
               )
             )
    ),
    tabPanel("Year Details", fluid = TRUE,
             br(),
             fluidRow(
               column(5,offset = 1,
                      sliderInput("year_for_details_left", label = "Select Year:", min = 1921, 
                                  max = 2020, value = 1921, step=1, sep = ""),
               ),
               column(5,offset = 1,
                      sliderInput("year_for_details_right", label = "Select Year:", min = 1921, 
                                  max = 2020, value = 2020, step=1, sep = ""),
               ),
             ),
             fluidRow(
               column(6, offset = 0,
                      plotOutput("year_radar_details_left"),
                      plotOutput("year_corr_details_left"),
                      plotOutput("year_music_key_details_left"),
               ),
               column(6, offset = 0,
                      plotOutput("year_radar_details_right"),
                      plotOutput("year_corr_details_right"),
                      plotOutput("year_music_key_details_right"),
               )
             ),
             fluidRow(
               column(8, offset = 2,
                      # plotOutput("year_radar_details_left"),
               )
             ),
             fluidRow(
               column(6, offset = 0,
                      #    plotOutput("year_corr_details_left")
               ),
               column(6,
                      #    plotOutput("year_music_key_details_left")
               ),
             ),
             
    )
    
    
  )
)


server <- function(input, output,session) {
  
  category_avg_year = reactive({ input$category_avg_year }) 
  year_avg_year = reactive({ input$year_avg_year })
  popularity_per_year = reactive({ input$popularity_per_year})
  artist_popularity_per_year = reactive({ input$artist_popularity_per_year })
  year_for_details_left = reactive({ input$year_for_details_left})
  year_for_details_right = reactive({ input$year_for_details_right})
  
  
  reactv = reactiveValues(plot_data = reload_data(clean,c("1921","2020"),0), choice=0 )
  
  observeEvent(input$year_avg_year,{
    reactv$plot_data <-  reload_data(clean,input$year_avg_year,reactv$choice)
  })
  
  observeEvent(input$btn_mean, {
    reactv$choice= 0
    updateButton(session,'btn_median',style = "secondary")
    updateButton(session,'btn_boxplot',style = "secondary")
    updateButton(session,'btn_mean',style = "primary")
  })
  
  observeEvent(input$btn_median, {
    reactv$choice= 1
    updateButton(session,'btn_median',style = "primary")
    updateButton(session,'btn_boxplot',style = "secondary")
    updateButton(session,'btn_mean',style = "secondary")
  })  
  
  observeEvent(input$btn_boxplot, {
    reactv$choice= 2
    updateButton(session,'btn_median',style = "secondary")
    updateButton(session,'btn_boxplot',style = "primary")
    updateButton(session,'btn_mean',style = "secondary")
  })  
  
  observeEvent(input$acoust, {
    switch_btn("acousticness")          
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  observeEvent(input$danceab, {
    switch_btn("danceability")   
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  observeEvent(input$dura, {
    switch_btn("duration_ms")   
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  observeEvent(input$engy, {
    switch_btn("energy")   
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  observeEvent(input$loud, {
    switch_btn("loudness") 
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  observeEvent(input$tempo, {
    switch_btn("tempo") 
    reactv$plot_data <- reload_data(clean,input$year_avg_year,reactv$choice)})
  
  output$avg_per_year_plot = renderPlot({ 
    
    if(reactv$choice == 0){
      ggplot(
        reactv$plot_data,aes(x=year,y=value, colour=variable))+
        geom_point(shape=18)+
        scale_color_manual(name = "Properties", 
                           values = color_coding)+
        geom_smooth()
    }
    else if(reactv$choice==1){
      ggplot(
        reactv$plot_data,aes(x=year,y=value, colour=variable))+
        geom_point(shape=18)+
        scale_color_manual(name = "Properties", 
                           values = color_coding)+
        geom_smooth()
    }
    else {
      ggplot(reactv$plot_data) + 
        geom_boxplot(aes(x=year, y=value, color=variable))+
        scale_color_manual(name = "Properties", 
                           values = color_coding)
    }
  })
  
  
  output$year_radar_details_left = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details_left()) -> selected_year_data
    selected_year_radar_data = subset(selected_year_data, select = -c(year, duration_s, key, loudness, tempo))
    
    selected_year_radar_data %>%
      summarise_at(colnames(selected_year_radar_data), mean, na.rm = TRUE) -> 
      selected_year_radar_data_means
    num_columns = length(colnames(selected_year_radar_data_means))
    radar_chart_data = rbind(rep(1, num_columns), rep(0, num_columns), selected_year_radar_data_means)
    
    op <- par(mar = c(1, 2, 2, 1))
    pretty_radarchart(radar_chart_data, caxislabels = c(0, 5, 10, 15, 20))
    par(op)
  })
  
  
  output$year_corr_details_left = renderPlot({
    
    detail_year_song_data %>% filter( year == year_for_details_left()) -> selected_year_data
    selected_year_data = subset(selected_year_data, select= -c(year))
    M = cor(selected_year_data)
    ggcorrplot(M,
               hc.order = FALSE, type = "lower",
               lab = TRUE,
               digits = 1,
               ggtheme = ggplot2::theme_dark(),
    )
    
    
  })
  
  output$year_music_key_details_left = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details_left()) -> selected_year_data
    selected_year_data = subset(selected_year_data, select= -c(year))
    
    ggplot( selected_year_data, aes(x=key)) +
      geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      ggtitle("Musical Key Histogram") +
      theme_ipsum() +
      theme(
        plot.title = element_text(size=15)
      ) + scale_x_continuous(breaks=0:11,
                             labels=c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B"))
  })
  
  output$year_radar_details_right = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details_right()) -> selected_year_data
    selected_year_radar_data = subset(selected_year_data, select = -c(year, duration_s, key, loudness, tempo))
    
    selected_year_radar_data %>%
      summarise_at(colnames(selected_year_radar_data), mean, na.rm = TRUE) -> 
      selected_year_radar_data_means
    num_columns = length(colnames(selected_year_radar_data_means))
    radar_chart_data = rbind(rep(1, num_columns), rep(0, num_columns), selected_year_radar_data_means)
    
    op <- par(mar = c(1, 2, 2, 1))
    pretty_radarchart(radar_chart_data, caxislabels = c(0, 5, 10, 15, 20))
    par(op)
  })
  
  
  output$year_corr_details_right = renderPlot({
    
    detail_year_song_data %>% filter( year == year_for_details_right()) -> selected_year_data
    selected_year_data = subset(selected_year_data, select= -c(year))
    M = cor(selected_year_data)
    ggcorrplot(M,
               hc.order = FALSE, type = "lower",
               lab = TRUE,
               digits = 1,
               ggtheme = ggplot2::theme_dark(),
    )
    
    
  })
  
  output$year_music_key_details_right = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details_right()) -> selected_year_data
    selected_year_data = subset(selected_year_data, select= -c(year))
    
    ggplot( selected_year_data, aes(x=key)) +
      geom_bar(fill="#69b3a2", color="#e9ecef", alpha=0.9) +
      ggtitle("Musical Key Histogram") +
      theme_ipsum() +
      theme(
        plot.title = element_text(size=15)
      ) + scale_x_continuous(breaks=0:11,
                             labels=c("C","C#","D","D#","E","F","F#","G","G#","A","A#","B"))
  })
  
  output$stats_about_artist_table <-  renderDT(df_search_table_Data,
                                               filter = "top",
                                               options = list(
                                                 pageLength = 50 ))
  
  
}


# Run app ----
shinyApp(ui, server)

