#
# Project Week 4

library(shiny)

shinyUI(fluidPage(
        titlePanel("Find Best Predictor Variables"),
        h4("Introduction"),
        h5("This application uses four different 
           databases available from R programming 
           language and uses two different statistical 
           methods to predict the conditional variable"),
        h4("Methods"),
        h5("I use a linear model to the best regressors 
           for predicting the conditional varible.  The 
           table contains the information about the best 
           regressors and the conditional variable is 
           also displayed."),
  sidebarLayout(
    sidebarPanel(
      sliderInput("sliderNumVar", "How many predictors do you want", 1, 10, value = 4, step = 1),
      checkboxInput("MTCARS", "mtcars data base", value = T),
      checkboxInput("AIRQUALITY", "airquality data base", value = F),
      checkboxInput("DIAMONDS", "diamonds data base", value = F),
      checkboxInput("STATE.X77", "state.x77 data base", value = F),
      selectInput("CRITERIA","Criteria",c("PVALUE" = 2,"AdjustedR" = 1)),
      actionButton("go", "Press to SUBMIT your choices")
      
    ),
    mainPanel(
      h2("Output From BestFit function "),
      h4("You can choose from four different datasets and from two different statistical criteria in order to determine which variables (regressors) best predicts the conditional variable"),
      h4("The table below displays the variables that best predict the conditional variable and indicates both the Pvalue and Adjusted Squared R value"),
      h4("After selecting your options you must press the SUBMIT button to proceed"),
      dataTableOutput("tabl"),
      h4(textOutput("text2")),
      h4(textOutput("text3")),
      h4(textOutput("text4"))
      
    )
  )
))
