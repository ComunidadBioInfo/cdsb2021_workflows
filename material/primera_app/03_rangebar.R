library(shiny)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(gapminder)

colorBy <- "continent"

ui <- fluidPage(
  # Application title
  titlePanel("Visualización exploratoria de gapminder"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      ## Código para seleccionar paises
      selectizeInput( 'countries', label = "Seleccionar paises", 
                      choices = levels(gapminder$country), multiple=TRUE),
      sliderInput( 'ylms', label = h3("Años contemplados"), min = 1960, 
                  max = 2020, value = c(1960, 2016) )
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
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2])) +
      labs(col=colorBy)
  })
}

# Run the application 
shinyApp(ui = ui, server = server)