#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

source('global.R')

# Define server logic required to draw a histogram
function(input, output) {
  
  # Define units
  units <- reactive({input$units})
  
  # Define minutes (pace)
  minutes <- reactive({
    make_minutes(input$pace)
  })
  
  # Create UI for pace
  output$pace <- renderUI(
    textInput('pace',
              'Pace',
              value = make_time(minutes() / 26.2)))
  # Create UI for total time
  output$total_time <- renderUI(
    textInput('total_time',
              'Total time',
              value = make_time(minutes() * 26.2)))
  
  # Table
  output$dt <- renderDataTable({
    f <- final()
    if(exists('f')){
      DT::datatable(f,
                    options = list(pageLength = nrow(final),
                                   buttons = c('copy', 'csv', 'excel', 'pdf', 'print'),
                                   dom = 'Bfrtip',
                                   fixedHeader = TRUE),
                    extensions = c('Buttons',
                                   'FixedHeader'),
                    rownames = FALSE)
    }
  })
}
