#
#Example2
#

library(shiny)

shinyUI(fluidPage(
  
  # Application title
  titlePanel("Plot random numbert"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
            numericInput("numeric","How many Numbers should be plotted?",
                         value=1000,min=1,max=1000,step=17),
       sliderInput("sliderx","Pick Min & max X value",
                   -100,100,value = c(-50,50)),
       sliderInput("slidery","pick min & max y value",
                   -100,100,value = c(-50,50)),
       checkboxInput("show_xlab","show/hide X axis label",value = TRUE),
       checkboxInput("show_ylab","show/hide y axis label",value = TRUE),
       checkboxInput("show_title","show/hide title")
    ),
    

    mainPanel(
            h3("Graph of Random points" ),
       plotOutput("plot1")
    )
  )
))
