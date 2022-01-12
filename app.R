# Load packages ----
library(shiny)
library(ggplot2)
library(dplyr)

# Source helper functions -----
source("helpers.R")

# Load data ----
df_music <- read.csv("data/data.csv")

# Clean and prep data ----
# TODO

# Make df reactive (faster loading when filtering on user inputs)
# TODO

years <- sort(unique(df_music$year))
# TODO Make dict and ensure that correct value is stored to its year + move to sever and make interactable
avg_per_year <- c()
for (i in seq(1,length(years), by=1)){
  avg_per_year[i] <- avg_maker('acousticness', 10, years[i])
}

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
    explo_var <- switch(input$var, 
                   "acousticness" = df_music$acousticness,
                   "danceability" = df_music$danceability,
                   "energy" = df_music$energy,
                   "instrumentalness" = df_music$instrumentalness,
                   "liveness" = df_music$liveness,
                   "loudness" = df_music$loudness,
                   "speechiness" = df_music$speechiness,
                   "tempo" = df_music$tempo,
                   "valence" = df_music$valence
                   )
    # TODO don't cheat on combination.. 
    plot.default(years, avg_per_year)
  })
  
}

# Run app ----
shinyApp(ui, server)
