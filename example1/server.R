#
#example1
#

library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$text1 <- renderText(input$slider2^2)
})

