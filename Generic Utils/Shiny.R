library(shiny)

# User interface ----
ui <- fluidPage(
  titlePanel('censusVis'),
  
  sidebarLayout(
    sidebarPanel(
      helpText('impact analysis'),
      
      selectInput('varNameOutput', 
                  label = 'choose a variable to analyse',
                  choices = c('Percent White', 'Percent Black',
                              'Percent Hispanic', 'Percent Asian'),
                  selected = 'Percent White'),
      
      sliderInput('range', 
                  label = 'Range of interest:',
                  min = 0, max = 100, value = c(0, 100))
    ),
    
    mainPanel(
      textOutput('varNameOutput')
    )
  )
)

# Server logic ----
server <- function(input, output) {
  
  output$varNameOutput <- renderText({ 
    paste("You have selected", input$varNameOutput)
  })
  
}

# Run app ----
shinyApp(ui, server)