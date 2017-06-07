#
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
                    
                    ifelse(z==1,criteria<-AdjR,criteria<-pval)                        }
            #if(z==1){criteria<-AdjR}
            #if(!z==1){criteria<-pval}                            
            LowToHigh <- order(criteria)
            LowToHigh<- LowToHigh[1:y]
            xLowval<-x1[,(LowToHigh)]
            HighToLow<-order(-criteria)
            HighToLow<-HighToLow[1:y]
            xHighval<-x1[,(HighToLow)]
            Regressor_lowest<-names(xLowval)
            Regressor_highest<-names(xHighval)
            AdjustedRSquaredH<-round(AdjR[HighToLow],4)
            PvaluesH<-signif(pval[HighToLow],4)
            AdjustedRSquaredL<-round(AdjR[LowToHigh],4)
            PvaluesL<-signif(pval[LowToHigh],4)
            aa1<-table(Regressor_lowest,AdjustedRSquaredL,PvaluesL) 
            aa2<-table(Regressor_highest,AdjustedRSquaredH,PvaluesH)
            aa3<-tbl_df(aa1)
            aa3<-filter(aa3,n>0)
            aa3<-select(aa3,-n)
            aa4<-tbl_df(aa2)
            aa4<-filter(aa4,n>0)
            aa4<-select(aa4,-n)
            aa3$PvaluesL<-as.numeric(aa3$PvaluesL)
            aa3$AdjustedRSquaredL<-as.numeric(aa3$AdjustedRSquaredL)
            aa4$PvaluesH<-as.numeric(aa4$PvaluesH)
            aa4$AdjustedRSquaredH<-as.numeric(aa4$AdjustedRSquaredH)
            aa5low<-as.data.frame(aa3)
            aa5high<-as.data.frame(aa4)
            ifelse(z==1,aa5<-aa5high,aa5<-aa5low)
            aa5
            ##print(aa5)
            ##print(s)
    }
    
    
    
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
            
            if(input$MTCARS){text1<-"mtcars database"
            }
            if(input$AIRQUALITY){text1<-"airquality database"
            }
            if(input$DIAMONDS){text1<-"diamonds database"
            }
            if(input$STATE.X77){text1<-"state.x77 database"
            }
            text1
   })   
    
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
            text4
    }) 
    
    
        output$tabl<-renderDataTable({react1()})
       
        output$text1 <- renderText({react2()
   })
        output$text2 <- renderText({paste("You have selected the", react2())
   })
        output$text3 <- renderText({paste("The method", react3())
   })
        output$text4 <- renderText({react4()
   })
   
   }) 
                
                
      

