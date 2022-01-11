# Load packages ----
library(shiny)

# Load data ----
df_music <- read.csv("data/data.csv")

# Source helper functions -----

# User interface ----
ui <- fluidPage(
  
  titlePanel("Music Explorer"),

)

# Server logic ----
server <- function(input, output) {
}

# Run app ----
shinyApp(ui, server)
