# General settings. ####
rm(list = ls())
library(tidyverse)
library(shiny)
library(shinydashboard)
library(shinyfullscreen)
source(
  'C:/Data Science Team/Projects/Automatic Data Analysis/Generic Utils/VisualiseImpact.R'
)
source(
  'C:/Data Science Team/Projects/Automatic Data Analysis/Generic Utils/CalculateImpact.R'
)
source(
  'C:/Data Science Team/Projects/Automatic Data Analysis/Generic Utils/CalculateImpact2.R'
)

fileNameDataRaw <-
  'C:/Data Science Team/Projects/Automatic Data Analysis/Kostprijsaardappelen/saldo aardappel 2006-2020.xlsx'


# Get the raw data. ####

dataRaw <-
  openxlsx::read.xlsx(fileNameDataRaw, sheet = 'DWH-MA', startRow = 5)
dataRaw <- dataRaw %>%
  filter(ID %in% c(20002, 20006, 20030, 20039, 20040, 20041, 20042, 20043, 20066)) %>%
  select(
    entity = Bedrijfsnummer,
    year = Jaar,
    regime = regimenaam,
    variable = naamnl,
    value = Uitkomst
  ) %>%
  mutate(
    regime = case_when(
      regime == 'biologisch dynamisch' ~ 'organic',
      regime == 'ecologisch' ~ 'ecological',
      regime == 'gangbaar' ~ 'conventional',
      regime == 'in omschakeling' ~ 'in transition'
    ),
    variable = case_when(
      variable == 'Andere kosten' ~ 'other costs',
      variable == 'Energie' ~ 'energy',
      variable == 'Gewasbeschermingsmiddelen' ~ 'plant protection',
      variable == 'Gewassen, bijproduct' ~ 'plant side products',
      variable == 'Gewicht bijproduct' ~ 'weight side products',
      variable == 'Gewicht bijproduct / ha' ~ 'weight side products per acre',
      variable == 'Gewicht hoofdproduct' ~ 'weight main product',
      variable == 'Gewicht hoofdproduct / ha' ~ 'weight main product per acre',
      variable == 'Hoofdproduct' ~ 'main product',
      variable == 'Loonwerk' ~ 'contract work',
      variable == 'Meststoffen' ~ 'manure',
      variable == 'Oppervlakte, totaal' ~ 'surface',
      variable == 'Overige' ~ 'remaining (other)',
      variable == 'Overige producten' ~ 'remaining products',
      variable == 'Overige toegerekende kosten' ~ 'remaining',
      variable == 'Prijs hoofdproduct, per 100 kg' ~ 'price per main product',
      variable == 'Saldo 1' ~ 'net result 1',
      variable == 'Saldo 2' ~ 'net result 2',
      variable == 'Schadevergoeding' ~ 'compensation',
      variable == 'Totaal opbrengsten' ~ 'total revenue',
      variable == 'Totaal toegerekende kosten' ~ 'total accrued costs per unit',
      variable == 'Totale opbrengsten' ~ 'total revenue',
      variable == 'Totale toegerekende kosten' ~ 'total accrued costs per farm',
      variable == 'Zaden, plant- en pootgoed' ~ 'reproduction material'
    )
  )
functionMappings <-
  rbind(
    # data.frame(
    #   output = 'weight main product per acre',
    #   input = c('weight main product', 'surface'),
    #   mapping = 'function(data) data$`weight main product` / data$surface',
    #   check.names = FALSE
    # ),
    # data.frame(
    #   output = 'weight side products per acre',
    #   input = c('weight side products', 'surface'),
    #   mapping = 'function(data) data$`weight side products` / data$surface',
    #   check.names = FALSE
    # ),
    data.frame(
      output = 'total accrued costs per unit',
      input = c('total accrued costs per farm', 'surface'),
      mapping = 'function(data) data$`total accrued costs per farm` / data$surface',
      derivative = c(
        'function(data, nthDerivative) ifelse(nthDerivative == 1, 1 / data$surface, 0)',
        'function(data, nthDerivative) (-1)^nthDerivative * factorial(nthDerivative) * data$`total accrued costs per farm` / data$surface^(nthDerivative + 1)'
      ),
      `maximum number of non-zero derivative` = c(1, Inf),
      check.names = FALSE
    ),
    data.frame(
      output = 'total accrued costs per farm',
      input = c(
        'plant protection',
        'manure',
        'reproduction material',
        'energy',
        'remaining'
      ),
      mapping = 'function(data) data$`plant protection` + data$manure + data$`reproduction material` + data$energy + data$remaining',
      derivative = rep( 'function(data, nthDerivative) 1', 5),
      `maximum number of non-zero derivative` = rep(1, 5),
      check.names = FALSE
    )
  )
# Some variables in the original dataset are mapped to the same renamed
# variables. Then there are duplicate values. Sum the values to retain only a
# unique set of values.
dataRaw <-
  dataRaw %>%
  group_by(across(-value)) %>%
  summarise(value = sum(value), .groups = 'drop') %>%
  arrange(entity, year, variable, regime)



# Perform the analysis. ####

varNamesOutputPlot <-
  unique(c(functionMappings$input, functionMappings$output))
varNamesDimension <- c('year', 'entity', 'regime')
varNamesDimensionPlot <-
  c('none', setdiff(varNamesDimension, c('year', 'entity')))
VisualiseImpact(dataRaw, "total accrued costs per farm", fontSize = 12)

sidebar <- dashboardSidebar(
  selectInput(
    'varNameOutput',
    label = 'output variable',
    choices = varNamesOutputPlot,
    selected = 'total accrued costs per unit'
  ),
  
  selectInput(
    'varNameDimension',
    label = 'dimension variable (optional)',
    choices = varNamesDimensionPlot,
    selected = varNamesDimensionPlot[1]
  ),
  
  numericInput(
    'maxNumInputsInPlot',
    label = 'number of individual input variables to visualise',
    value = 3,
    min = 2
  ),
  
  checkboxInput(
    'isShowConfidenceInterval',
    'show confidence intervals?',
    value = FALSE
  ),
  
  numericInput(
    'fontSize',
    label = 'font size of the text on the axis',
    value = 18,
    min = 0
  ),
  
  collapsed = TRUE
)
ui <- dashboardPage(dashboardHeader(title = 'impact analysis'),
                    sidebar,
                    dashboardBody(fillPage(
                      tags$style(type = 'text/css', '#plot {height: calc(100vh - 80px) !important;}'),
                      plotOutput('plot', width = '100%', height = '100%')
                    )))
server <- function(input, output, session) {
  output$plot <- renderPlot({
    #reactivePlot
    if (input$varNameDimension == 'none') {
      varNameDimension <- NULL
    } else{
      varNameDimension <- input$varNameDimension
    }
    VisualiseImpact(
      dataRaw,
      input$varNameOutput,
      varNameDimension,
      maxNumInputsInPlot = input$maxNumInputsInPlot,
      isShowConfidenceInterval = input$isShowConfidenceInterval,
      fontSize = input$fontSize
    )
  })
}
shinyApp(ui, server)