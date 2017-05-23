#
#Example2
#

library(shiny)

shinyServer(function(input, output) {
   
  output$plot1 <- renderPlot({
    
    set.seed(445)
   number_of_points<-input$numeric
   minX<-input$sliderx[1]
   maxX<-input$sliderx[2]
   minY<-input$slidery[1]
   maxY<-input$slidery[2]
   dataX<-runif(number_of_points,minX,maxX)
   dataY<-runif(number_of_points,minY,maxY)
   xlab<-ifelse(input$show_xlab,"X Axis","")
        ylab<-ifelse(input$show_ylab,"Y Axis","")
        
        main<-ifelse(input$show_title,"Title","")
        plot(dataX,dataY,xlab=xlab,ylab=ylab,main=main,
             xlim = c(-100,100),ylim=c(-100,100))
   
    
  })
  
})
