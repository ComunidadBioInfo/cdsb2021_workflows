library(shiny)
library(dslabs)
library(magrittr)
library(ggplot2)
library(tidyverse)
library(ggridges)
data(gapminder)

ui <- fluidPage(
  # Application title
  titlePanel("Visualización exploratoria de gapminder"),
  # Sidebar for parameters
  sidebarLayout(
    sidebarPanel(
      ## Código para seleccionar paises
      selectizeInput( 'countries', label = h3("Seleccionar paises"), 
                      choices = levels(gapminder$country), multiple=TRUE),
      sliderInput( 'ylms', label = h3("Años contemplados"), min = 1960, 
                   max = 2020, value = c(1960, 2016) ),
      radioButtons( 'colorBy', label=h3("Colorear por"), 
                    choices = c(pais="country", continente="continent"), 
                    selected = "continent" )
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Esperanza de vida", 
                 plotOutput("lifeExpPlot"),
                 textOutput("plotLegend")),
        tabPanel("Fertilidad", 
                 plotOutput("fertilityPlot")),
        tabPanel("Riqueza sobre tiempo", 
                 plotOutput("ridgesPlot"))
      )
    )
  )
)

# Define server logic
server <- function(input, output) {
  gapminderSub <- reactive({
    gapminder %>%
      filter( country %in% input$countries )
  })
  output$lifeExpPlot <- renderPlot({
    gapminderSub() %>%  
      ggplot( aes( year, life_expectancy, col=get(input$colorBy), group=country )) +
      geom_point( size=0.3 ) +
      geom_line( ) +
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2])) +
      labs(col=input$colorBy)
  })
  output$plotLegend <- renderText({ 
    sprintf("Datos de esperanza de vida del año %d al año %d. Cada color 
    representa un %s. Los siguientes países estan representados en el gráfico: %s.",
            input$ylms[1], input$ylms[2], 
            ifelse(input$colorBy == "continent", "continente", "país"), 
            paste(input$countries, collapse=", "))
  })
  output$fertilityPlot <- renderPlot({
    gapminderSub() %>%
      ggplot( aes( year, fertility, col=get(input$colorBy), group=country )) +
      geom_point( size=0.3 ) +
      geom_line( ) +
      coord_cartesian(xlim=c(input$ylms[1], input$ylms[2])) +
      labs(col=input$colorBy)
  })
  output$ridgesPlot <- renderPlot({
    keep_regions <- gapminderSub() %>%
      pull( region ) %>%
      as.character %>%
      unique
    gapminder %>%
      mutate( dollars_per_day=gdp/population/365 ) %>%
      filter( year %in% seq(1960, 2010, 10), 
              !is.na(dollars_per_day), 
              region %in% keep_regions ) %>%
      filter( between(year, input$ylms[1], input$ylms[2]) ) %>%
      ggplot( aes( dollars_per_day, factor(year) ) ) +
      scale_x_continuous(trans = "log2")  +
      geom_density_ridges(jittered_points = TRUE) +
      facet_grid( ~region, scales="free")
  })
}

# Run the application 
shinyApp(ui = ui, server = server)