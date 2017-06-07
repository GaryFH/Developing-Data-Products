`##{r setup, include=FALSE} ##knitr::opts_chunk$set(echo = FALSE)`

Find best regressors set up function called "BestFitVar" (an x,y,z function) - note for this to work the data must be in a data.frame format and the dependent variable needs to be the first column.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### x= database, y= number of best regressors you want to find, z= if 1 then Adusted R squared is criteria for choosing the best regressors - any other value for z means Pvalue is the criteria method

    library(caret);library(dplyr);library(ggplot2)

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

Example using mtcars database
-----------------------------

### Put dependent variable in first column

    BestFit(mtcars,4,1)  ##Adusted R squared criteria

    ##   Regressor_highest AdjustedRSquaredH  PvaluesH
    ## 1                wt            0.7446 1.294e-10
    ## 2               cyl            0.7171 6.113e-10
    ## 3              disp            0.7090 9.380e-10
    ## 4                hp            0.5892 1.788e-07

    BestFit(mtcars,4,2)  ##Pvalue criteria

    ##   Regressor_lowest AdjustedRSquaredL  PvaluesL
    ## 1               wt            0.7446 1.294e-10
    ## 2              cyl            0.7171 6.113e-10
    ## 3             disp            0.7090 9.380e-10
    ## 4               hp            0.5892 1.788e-07

Example using diamonds database (part of ggplot2)
-------------------------------------------------

### Put dependent variable in first column and remove unwanted variables (x,y,z)

    d1<-tbl_df(diamonds)
    d2<-select(d1,price,carat:table) ## price first column
    d2<-as.data.frame(d2)

    ###next put in function x=database,y=numberof best variables you want to find
    BestFit(d2,4,2) ##Pvalue criteria

    ##   Regressor_lowest AdjustedRSquaredL   PvaluesL
    ## 1            carat            0.8493  0.000e+00
    ## 2            color            0.0312 1.338e-300
    ## 3            table            0.0161 3.762e-193
    ## 4          clarity            0.0270  4.696e-68

    BestFit(d2,4,1) ##Adusted R squared criteria

    ##   Regressor_highest AdjustedRSquaredH   PvaluesH
    ## 1             carat            0.8493  0.000e+00
    ## 2             color            0.0312 1.338e-300
    ## 3             table            0.0161 3.762e-193
    ## 4           clarity            0.0270  4.696e-68

Example using airquality database - no prep is required
-------------------------------------------------------

### Put dependent variable in first column

    BestFit(airquality,4,2) ##Pvalue criteria

    ##   Regressor_lowest AdjustedRSquaredL  PvaluesL
    ## 1             Temp            0.4832 2.932e-18
    ## 2             Wind            0.3563 9.272e-13
    ## 3          Solar.R            0.1133 1.793e-04
    ## 4            Month            0.0185 7.760e-02

    BestFit(airquality,4,1)##Adusted R squared criteria

    ##   Regressor_highest AdjustedRSquaredH  PvaluesH
    ## 1              Temp            0.4832 2.932e-18
    ## 2              Wind            0.3563 9.272e-13
    ## 3           Solar.R            0.1133 1.793e-04
    ## 4             Month            0.0185 7.760e-02

\`\`
----

Example using state database
----------------------------

### Put dependent variable in first column

    state<-as.data.frame(state.x77)
    BestFit(state,5,2) ##Pvalue criteria

    ##   Regressor_lowest AdjustedRSquaredL PvaluesL
    ## 1           Murder            0.0997  0.01455
    ## 2            Frost            0.0918  0.01844
    ## 3           Income            0.0234  0.14670
    ## 4       Illiteracy           -0.0090  0.45690
    ## 5          HS Grad           -0.0109  0.49620

    BestFit(state,5,1)##Adusted R squared criteria

    ##   Regressor_highest AdjustedRSquaredH PvaluesH
    ## 1            Murder            0.0997  0.01455
    ## 2             Frost            0.0918  0.01844
    ## 3            Income            0.0234  0.14670
    ## 4        Illiteracy           -0.0090  0.45690
    ## 5           HS Grad           -0.0109  0.49620
