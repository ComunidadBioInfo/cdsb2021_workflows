library(shiny)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(gapminder)

ymin <- 1960
ymax <- 2016
colorBy <- "continent"

ui <- fluidPage(
  # Application title
  titlePanel("Visualización exploratoria de gapminder"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      ## Código para seleccionar paises
      selectizeInput( 'countries', label = "Seleccionar paises", 
                      choices = levels(gapminder$country), multiple=TRUE)
    ),
    mainPanel(
      plotOutput("lifeExpPlot")
    )
  )
)

# Define server logic
server <- function(input, output) {
  output$lifeExpPlot <- renderPlot({
    gapminder %>%
      filter( country %in% input$countries ) %>%
      ggplot( aes( year, life_expectancy, col=get(colorBy), group=country )) +
      geom_point( size=0.3 ) +
      geom_line( ) +
      xlim(c(ymin, ymax)) +
      labs(col=colorBy)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
