`##{r setup, include=FALSE} ##knitr::opts_chunk$set(echo = FALSE)`

Find best regressors set up function called "BestFitVar" (an x,y,z function) - note for this to work the data must be in a data.frame format and the dependent variable needs to be the first column.
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### x= database, y= number of best regressors you want to find, z= if 1 then Adusted R squared is criteria for choosing the best regressors - any other value for z means Pvalue is the criteria method

    library(caret);library(dplyr);library(ggplot2)

    BestFitVar<-function(x,y,z) {
             
             
    ## Remove near zero and mostly NA variables - Near zero only works if all variables are numeric!

                            ## nza<-nearZeroVar(x) - 
                            ## x<-x[,-nza]
            mostNA<-sapply(x, function(x) mean(is.na(x)))>=.95
            x<-x[,mostNA==F]

    ## Remove dependent variable from potential regressors                
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
                            #print(names(s))
                            #print(s)
                            #print(class(pval))
                            #print(pval)
                            #regname<-names(x1)
                            #print(regname)
                  }
    ## make new data.frame by removing extra regressors 
            if(z==1){criteria<-AdjR}
            if(!z==1){criteria<-pval} 
                    
            LowToHigh <- order(criteria)
            LowToHigh<- LowToHigh[1:y]
            xLowval<-x1[,(LowToHigh)]
            Regressor_lowest<-names(xLowval)
                            #print(LowToHigh)
                            #print(names(xLow))
                            #print(a1)
                    
     #prepare and print
                                    
            AdjustedRSquared<-round(AdjR[LowToHigh],4)
            Pvalues<-signif(pval[LowToHigh],4)
                    a1<-table(Regressor_lowest,AdjustedRSquared,Pvalues)
                    a2<-tbl_df(a1)
                    a2$Pvalues<-as.numeric(a2$Pvalues)
                    a2$AdjustedRSquared<-as.numeric(a2$AdjustedRSquared)
                    a3<-filter(a2,n>0)
                    a4<-select(a3,-n)
                    ##a4<-arrange(a4,Pvalues)
                    a5<-as.matrix(a4)
                    print(a5,quote = F)
                    rownames(a5)<-rep("",nrow(a5))
            ##
    }

Example using mtcars database
-----------------------------

### Put dependent variable in first column

    BestFitVar(mtcars,4,1)  ##Adusted R squared criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues 
    ## [1,] am               0.3385           0.000285
    ## [2,] carb             0.2803           0.001084
    ## [3,] gear             0.2050           0.005401
    ## [4,] qsec             0.1478           0.017080

    BestFitVar(mtcars,4,2)  ##Pvalue criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues  
    ## [1,] wt               0.7446           1.294e-10
    ## [2,] cyl              0.7171           6.113e-10
    ## [3,] disp             0.7090           9.380e-10
    ## [4,] hp               0.5892           1.788e-07

Example using diamonds database (part of ggplot2)
-------------------------------------------------

### Put dependent variable in first column and remove unwanted variables (x,y,z)

    d1<-tbl_df(diamonds)
    d2<-select(d1,price,carat:table) ## price first column
    d2<-as.data.frame(d2)

    ###next put in function x=database,y=numberof best variables you want to find
    BestFitVar(d2,4,2) ##Pvalue criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues   
    ## [1,] carat            0.8493            0.000e+00
    ## [2,] color            0.0312           1.338e-300
    ## [3,] table            0.0161           3.762e-193
    ## [4,] clarity          0.0270            4.696e-68

    BestFitVar(d2,4,1) ##Adusted R squared criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues   
    ## [1,] table            0.0161           3.762e-193
    ## [2,] clarity          0.0270            4.696e-68
    ## [3,] cut              0.0128            9.803e-08
    ## [4,] depth            0.0001            1.340e-02

Example using airquality database - no prep is required
-------------------------------------------------------

### Put dependent variable in first column

    BestFitVar(airquality,4,2) ##Pvalue criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues  
    ## [1,] Temp             0.4832           2.932e-18
    ## [2,] Wind             0.3563           9.272e-13
    ## [3,] Solar.R          0.1133           1.793e-04
    ## [4,] Month            0.0185           7.760e-02

    BestFitVar(airquality,4,1)##Adusted R squared criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues  
    ## [1,] Wind              0.3563          9.272e-13
    ## [2,] Solar.R           0.1133          1.793e-04
    ## [3,] Month             0.0185          7.760e-02
    ## [4,] Day              -0.0086          8.879e-01

\`\`
----

Example using state database
----------------------------

### Put dependent variable in first column

    state<-as.data.frame(state.x77)
    BestFitVar(state,5,2) ##Pvalue criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues
    ## [1,] Murder            0.0997          0.01455
    ## [2,] Frost             0.0918          0.01844
    ## [3,] Income            0.0234          0.14670
    ## [4,] Illiteracy       -0.0090          0.45690
    ## [5,] HS Grad          -0.0109          0.49620

    BestFitVar(state,5,1)##Adusted R squared criteria

    ##      Regressor_lowest AdjustedRSquared Pvalues
    ## [1,] Income            0.0234          0.1467 
    ## [2,] Illiteracy       -0.0090          0.4569 
    ## [3,] HS Grad          -0.0109          0.4962 
    ## [4,] Life Exp         -0.0161          0.6387 
    ## [5,] Area             -0.0203          0.8765