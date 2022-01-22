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
  
  output$stats_about_artist_table <-  DT::renderDataTable(datatable(df_song_metadata))
  
  
}



# Run app ----
shinyApp(ui, server)

