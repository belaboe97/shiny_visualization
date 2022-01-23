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
library(ggcorrplot)
library(corrplot)

library(tidyverse)
library(hrbrthemes)
library(fmsb)

# Source helper functions -----
#source("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/helpers.R")
#source("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/styles.R")
source("helpers.R")
source("styles.R")
# Load data ----
#df_music =  read.csv("C:/Users/Bela Boente/Desktop/Programming/DataVisualization/music_explorer/data/data.csv")
df_music = read.csv("data/data.csv")

# Clean and prep data ----
# TODO

clean<-clean_names(df_music)
clean$duration_s<- clean$duration_ms/1000



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

clean %>% 
  select(c("acousticness", "danceability", "duration_s", "energy", "explicit", "instrumentalness", "key", "liveness", 
           "loudness", "mode" , "speechiness", "tempo", "valence", "year")) -> 
  detail_year_song_data 


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
             
             br(),
             sidebarLayout(
               
               sidebarPanel(
                 
                 
                 p("All this variables are related to songs itself"),
                 strong("Choose category"),
                 
                 selectInput("category_avg_year", 
                             label = "Select a variable to examine.",
                             choices= colnames(df_song_data),
                             selected = "acousticness"),
                 
                 strong("Choose year"),
                 sliderInput("year_avg_year", label = "Years", min = 1921, 
                             max = 2021, value = c(1921, 2021)),
                 br(),
                 br(),
                 br(),
                 br(),
                 hr(),
                 
                 p("Search for the most common songs per year"),
                 
                 textInput("popularity_per_year", "Choose year of intrest", value = "2018", width = NULL),
                 textInput("artist_popularity_per_year", "Choose artist of intrest", value = "Tyga", width = NULL), 
               ),
               
               
               mainPanel(
                 h4("Song related data"),
                 plotOutput("avg_per_year_plot"),
                 h4("Most popular artists per year"),
                 plotOutput("most_popular_songs"),
                 
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
                      DT::dataTableOutput("stats_about_artist_table")
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
                      plotOutput("year_music_key_details_left")
               ),
               column(6, offset = 0,
                      plotOutput("year_radar_details_right"),
                      plotOutput("year_corr_details_right"),
                      plotOutput("year_music_key_details_right")
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
             
    ),
    navbarMenu("More",
               tabPanel("Sub-Component A"),
               tabPanel("Sub-Component B"))
    
    
  )
)

# Server logic ----
server <- function(input, output) {
  
  category_avg_year = reactive({ input$category_avg_year }) 
  year_avg_year = reactive({ input$year_avg_year })
  popularity_per_year = reactive({ input$popularity_per_year})
  artist_popularity_per_year = reactive({ input$artist_popularity_per_year })
  year_for_details_left = reactive({ input$year_for_details_left})
  year_for_details_right = reactive({ input$year_for_details_right})
  
  
  output$avg_per_year_plot = renderPlot({ 
    
    plot.default(
      avg_maker(clean, 
                category_avg_year(), 
                year_avg_year()))
  })
  
  
  
  output$most_popular_songs = renderPlot({  
    mps = most_popular_songs(clean, popularity_per_year())
    mps_df = data.frame(artists = mps$artists, popularity = mps$popularity, name = mps$name)
    
    mps_artist = search_artist_mps(clean, artist_popularity_per_year(), popularity_per_year())
    mps_artist_df = data.frame(artists = mps_artist$artists, 
                               popularity = mps_artist$popularity, name = mps_artist$name)
    
    new_df = bind_rows(mps_df,mps_artist_df)
    print(new_df)
    
    ggplot(new_df, aes(x=artists,y = popularity), environment=environment()) + 
      geom_bar(stat = "identity",fill="darkblue", color="white") + 
      geom_text(aes(label = name), vjust = 1.5, colour = "white")
    
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
  
  output$stats_about_artist_table <-  DT::renderDataTable(datatable(df_song_metadata))
  
  
}





# Run app ----
shinyApp(ui, server)

