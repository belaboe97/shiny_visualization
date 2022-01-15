library(highcharter)
library(shiny)
library(dplyr)

df = data.frame(
  team = c("Arsenal", "Chelsea", "Liverpool"),
  wins = c(533, 489, 584),
  loss = c(120, 201, 186),
  draws = c(156, 153, 246)
)

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("Football Analysis"),
  
  sidebarLayout(
    sidebarPanel(
      selectizeInput("teams", "Teams", choices = unique(df$team), selected = unique(df$team)[1])
    ),
    
    mainPanel(
      highchartOutput("plot")
    )
  )
)

server <- function(input, output) {
  
  reactivedf <- reactive({
    filtereddf <- df %>%
      dplyr::filter(team == input$teams)
    filtereddf
  })
  
  output$plot <- renderHighchart({
    highchart() %>%
      hc_add_series(type = "column", reactivedf()$wins, name = "wins") %>%
      hc_add_series(type = "column", reactivedf()$loss, name = "loss") %>%
      hc_add_series(type = "column", reactivedf()$draws, name = "draws") %>%
      hc_xAxis(labels = list(enabled = FALSE)) %>%
      hc_title(text = input$teams)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)