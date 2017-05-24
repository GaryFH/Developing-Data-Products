#    example1
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(
        
        # Application title
        titlePanel("Old Faithful Geyser Data"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
                sidebarPanel(
                        h3("sidebar text"),
                        h1("different size text"),
                        h2("different size"),
                        em("Emphasized text"),
                        sliderInput("slider2","slide me",0,100,0) 
                ),
                mainPanel(
                        h3("slider value"),
                        textOutput("text1")
                        
                )
        )
))


