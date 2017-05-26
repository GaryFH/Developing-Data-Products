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
        a1<-mtcars
        a2<-airquality
        a3<-as.data.frame(d2)##diamonds
        a4<-state<-as.data.frame(state.x77)
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
                ## rownames(aa5)<-rep("",nrow(aa5))
        }       
        
          
       react1<- reactive({ 
                
        if(input$AdjustedR) {z<-1
        } 
        if(input$PVALUE) {z<-2
        }   
        if(input$MTCARS){BestFit(mtcars,Input$sliderNumVar,z)
        }
        if(input$AIRQUALITY){BestFit(airquality,Input$sliderNumVar,z)
        }
        if(input$DIAMONDS){BestFit(diamonds,Input$sliderNumVar,z)
        }
        if(input$STATE.X77){BestFit(state<-as.data.frame(state.x77),Input$sliderNumVar,z)
        }
     
       })      
            
                
                output$tabl<-renderDataTable(react1,
                        options = list(
                                pageLength = 5,
                                initComplete = I("function(settings, json) {alert('Done.');}")
                        )
                )
                }
                ) 
                
                
      

