#
# Project Week 4

library(shiny)

shinyUI(fluidPage(
        titlePanel("Find Best Predictor Variables"),
        sidebarLayout(
                sidebarPanel(
        sliderInput("sliderNumVar", "How many predictors do you want", 1, 4, value = 2, step = 1),
        checkboxInput("MTCARS", "mtcars data base", value = T),
        checkboxInput("AIRQUALITY", "airquality data base", value = F),
        checkboxInput("DIAMONDS", "diamonds data base", value = F),
        checkboxInput("STATE.X77", "state.x77 data base", value = F),
        checkboxInput("PVALUE", "criteria-Pvalue", value = F),
        checkboxInput("AdjustedR", "criteria-adjusted r squared", value = T),
        submitButton("Submit")              
                ),
                
                mainPanel(
                       
                        dataTableOutput("react1")
                )
        )
))
