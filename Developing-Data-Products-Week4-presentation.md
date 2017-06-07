Developing-Data-Products-Week4-presentation
========================================================
author: GaryFH 
date: 05/27/2017
autosize: true


BestFit Shiny Application  Find Best Predictor Variables
========================================================
- Introduction

This application uses four different databases available from R programming language and uses two different statistical methods to predict the conditional variable.  A basic understanding of statistical methods is required to understand the output.

- Methods

The application uses a linear model to the best regressors for predicting the conditional varible. The table contains the information about the best regressors and the conditional variable is also displayed.   Note that the code is easy to modify to change or add databases.   It can also be easily modified to add or change criteria used for selecting the best available regressors.
- https://github.com/GaryFH/Developing-Data-Products

Example of embedded R code that gets run presenting the document
========================================================
<!--html_preserve--><div class="container-fluid">
<h2>Find Best Predictor Variables</h2>
<h4>Introduction</h4>
<h5>This application uses four different 
           databases available from R programming 
           language and uses two different statistical 
           methods to predict the conditional variable</h5>
<h4>Methods</h4>
<h5>I use a linear model to the best regressors 
           for predicting the conditional varible.  The 
           table contains the information about the best 
           regressors and the conditional variable is 
           also displayed.</h5>
<div class="row">
<div class="col-sm-4">
<form class="well">
<div class="form-group shiny-input-container">
<label class="control-label" for="sliderNumVar">How many predictors do you want</label>
<input class="js-range-slider" id="sliderNumVar" data-min="1" data-max="10" data-from="4" data-step="1" data-grid="true" data-grid-num="9" data-grid-snap="false" data-prettify-separator="," data-prettify-enabled="true" data-keyboard="true" data-keyboard-step="11.1111111111111" data-data-type="number"/>
</div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="MTCARS" type="checkbox" checked="checked"/>
<span>mtcars data base</span>
</label>
</div>
</div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="AIRQUALITY" type="checkbox"/>
<span>airquality data base</span>
</label>
</div>
</div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="DIAMONDS" type="checkbox"/>
<span>diamonds data base</span>
</label>
</div>
</div>
<div class="form-group shiny-input-container">
<div class="checkbox">
<label>
<input id="STATE.X77" type="checkbox"/>
<span>state.x77 data base</span>
</label>
</div>
</div>
<div class="form-group shiny-input-container">
<label class="control-label" for="CRITERIA">Criteria</label>
<div>
<select id="CRITERIA"><option value="2" selected>PVALUE</option>
<option value="1">AdjustedR</option></select>
<script type="application/json" data-for="CRITERIA" data-nonempty="">{}</script>
</div>
</div>
<button id="go" type="button" class="btn btn-default action-button">Press to SUBMIT your choices</button>
</form>
</div>
<div class="col-sm-8">
<h2>Output From BestFit function </h2>
<h4>You can choose from four different datasets and from two different statistical criteria in order to determine which variables (regressors) best predicts the conditional variable</h4>
<h4>The table below displays the variables that best predict the conditional variable and indicates both the Pvalue and Adjusted Squared R value</h4>
<h4>After selecting your options you must press the SUBMIT button to proceed</h4>
<div id="tabl" class="shiny-datatable-output"></div>
<h4>
<div id="text2" class="shiny-text-output"></div>
</h4>
<h4>
<div id="text3" class="shiny-text-output"></div>
</h4>
<h4>
<div id="text4" class="shiny-text-output"></div>
</h4>
</div>
</div>
</div><!--/html_preserve-->
Sample of the Basic code of shiny application using code 
========================================================

```r
#Project week 4
library(shiny)
library(caret)
library(dplyr)
library(ggplot2)
shinyServer(function(input, output){      
    d1<-tbl_df(diamonds)
    d2<-select(d1,price,carat:table) ## made price first column
    diamonds<-as.data.frame(d2)
    BestFit<-function(x,y,z) {               
      x1<-x[,-1]
      ## Insure that the number of good regressors wanted does not exceed the number available
      NumOfRegressors<-ncol(x1)
      if(NumOfRegressors<y) stop("there are less than (y) regressors")
      pval<-numeric(NumOfRegressors)
      AdjR<-numeric(NumOfRegressors)
      ## Find best regressors (those with lowest P-values)
      for(i in seq_len(NumOfRegressors)) {
        fit<-lm(x[,1]~x1[,i])
        s<-summary(fit)
        pval[i]<-s$coefficients[2,4]
        AdjR[i]<-s$adj.r.squared
      }
      if(z==1){criteria<-AdjR}
      if(!z==1){criteria<-pval}
      LowToHigh <- order(criteria)
      LowToHigh<- LowToHigh[1:y]
      xLowval<-x1[,(LowToHigh)]
      Regressor_lowest<-names(xLowval)
      AdjustedRSquared<-round(AdjR[LowToHigh],4)
      Pvalues<-signif(pval[LowToHigh],4)
      aa1<-table(Regressor_lowest,AdjustedRSquared,Pvalues)
      aa2<-tbl_df(aa1)
      aa2$Pvalues<-as.numeric(aa2$Pvalues)
      aa2$AdjustedRSquared<-as.numeric(aa2$AdjustedRSquared)
      aa3<-filter(aa2,n>0)
      aa4<-select(aa3,-n)
      aa4<-arrange(aa4,Pvalues)
      aa5<-as.data.frame(aa4)
      ##print(aa5)
      ##aa5<-as.matrix(aa4)
      ##tableOutput("aa5")     
      ##print(aa5,quote = F)
      ## rownames(aa5)<-rep("",nrow(aa5))  }
    react1<- eventReactive(input$go,{ 
      if(input$MTCARS){tabl1<-BestFit(mtcars,input$sliderNumVar,input$CRITERIA)
      }
      if(input$AIRQUALITY){tabl1<-BestFit(airquality,input$sliderNumVar,input$CRITERIA)
      }
      if(input$DIAMONDS){tabl1<-BestFit(diamonds,input$sliderNumVar,input$CRITERIA)
      }
      if(input$STATE.X77){tabl1<-BestFit(state<-as.data.frame(state.x77),input$sliderNumVar,input$CRITERIA)
      }
      tabl1
    }) 
    react2<- reactive({ 
            if(input$MTCARS){text1<-"mtcars database"            }
            if(input$AIRQUALITY){text1<-"airquality database"            }
            if(input$DIAMONDS){text1<-"diamonds database"
            }
            if(input$STATE.X77){text1<-"state.x77 database"
            }
            text1   }) 
    react3<- reactive({ 
            ifelse(input$CRITERIA==1,text3<-"criteria is Adjusted R Squared",text3<-"Criteria is P Value")
            text3
    }) 
    react4<- reactive({ 
            if(input$MTCARS){text4<-"Conditional Variable is Miles per gallon"
            }
            if(input$AIRQUALITY){text4<-"Conditional Variable is Ozone level"
            }
            if(input$DIAMONDS){text4<-"Conditional Variable is Price"
            }
            if(input$STATE.X77){text4<-"Conditional Variable is Population"
            }
            text4    })
        output$tabl<-renderDataTable({react1()})
        output$text1 <- renderText({react2()   })
        output$text2 <- renderText({paste("You have selected the", react2())   })
        output$text3 <- renderText({paste("The method", react3())   })
        output$text4 <- renderText({react4()   })
   }) 
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
    )  )
))
```
    Summary
========================================================


- This is a demonstration project to give the user an idea of some of the ways Rstudio Presenter and Shiny can be used.

- The code requires an R data.frame database
- The code requires that the conditional variable be in the first column of the data.frame
- A submit button is required to produce the table
- The function allows the user to select the number of best fit regressors on a slider from 1 to 10 and the function allows the user to select the database to be used on check boxes - note that it is best to uncheck the previous box before submitting the new criteria.
- The function also allows the user to which statistical criteria to use in determining which regressors are the best at prediction.

