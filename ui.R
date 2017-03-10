library(shinythemes)
library(shinyLP)

ui <-
  navbarPage(
    title = 'Marathon pace',
    theme = shinytheme("flatly"),
    
    
    tabPanel('Home',
             h3('Welcome!'),
             fluidRow(
               column(4,
                      radioButtons('units',
                                         'Units: ',
                                         choices = c('Feet',
                                                     'Kilometers')),
                      uiOutput('pace'),
                      submitButton("Submit"),
                      uiOutput('total_time'),
                      submitButton("Submit")),
               column(8, 'bla'))),
    tabPanel('Basic analysis',
             fluidRow(column(12,
                             h1('text')))))