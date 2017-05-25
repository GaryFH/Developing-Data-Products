#
# Project Week 4

library(shiny)

shinyUI(fluidPage(
        titlePanel("Find Best Predictor Variables"),
        sidebarLayout(
                sidebarPanel(
        sliderInput("sliderNumVar", "How many predictors do you want",value = 2, 1, 4, step = 1),
        checkboxInput("MTCARS", "mtcars data base", value = TRUE),
        checkboxInput("AIRQUALITY", "airquality data base", value = F),
        checkboxInput("DIAMONDS", "diamonds data base", value = F),
        checkboxInput("STATE.X77", "state.x77 data base", value = F),
        checkboxInput("PVALUE", "criteria-Pvalue", value = F),
        checkboxInput("AdjustedR", "criteria-adjusted r squared", value = T)
                       
                ),
                
                mainPanel(
                       
                        tableOutput("aa5")
                )
        )
))
