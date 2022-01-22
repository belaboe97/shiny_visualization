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
               column(6,offset = 4,
                      sliderInput("year_for_details", label = "Years", min = 1921, 
                                  max = 2020, value = 2000, step=1, sep = ""),
               )
             ),
             fluidRow(
               column(8, offset = 2,
                      plotOutput("year_radar_details"),
               )
             ),
             fluidRow(
               column(6, offset = 0,
                      plotOutput("year_corr_details")
               ),
               column(6,
                      plotOutput("year_music_key_details")
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
  year_for_details = reactive({ input$year_for_details})
  
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
  
  output$year_radar_details = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details()) -> selected_year_data
    selected_year_radar_data = subset(selected_year_data, select = -c(year, duration_s, key, loudness, tempo))
    
    selected_year_radar_data %>%
      summarise_at(colnames(selected_year_radar_data), mean, na.rm = TRUE) -> 
      selected_year_radar_data_means
    num_columns = length(colnames(selected_year_radar_data_means))
    radar_chart_data = rbind(rep(1, num_columns), rep(0, num_columns), selected_year_radar_data_means)
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
    
    op <- par(mar = c(1, 2, 2, 1))
    pretty_radarchart(radar_chart_data, caxislabels = c(0, 5, 10, 15, 20))
    par(op)
  })
  
  
  output$year_corr_details = renderPlot({
    print(year_for_details())
    print(category_avg_year())
    
    detail_year_song_data %>% filter( year == year_for_details()) -> selected_year_data
    selected_year_data = subset(selected_year_data, select= -c(year))
    M = cor(selected_year_data)
    #corrplot(M, method = 'square', order = 'FPC', diag = FALSE)
    #corrplot.mixed(M, order = 'alphabet', tl.pos='lb')
    ggcorrplot(M,
               hc.order = FALSE, type = "lower",
               lab = TRUE,
               digits = 1,
               ggtheme = ggplot2::theme_dark(),
    )
    
    
  })
  
  output$year_music_key_details = renderPlot({
    detail_year_song_data %>% filter( year == year_for_details()) -> selected_year_data
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

