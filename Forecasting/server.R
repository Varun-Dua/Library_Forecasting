#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(openxlsx)
library(tidyr)
library(dplyr)
library('forecast')
library('tseries')

ETLRequest <- function(){
    # Reading data on Requests
    LibraryRequestData <- read.xlsx(xlsxFile = "www/MSc. Business Analytics - Capstone Project Requests Main Library.xlsx", fillMergedCells = TRUE, colNames = TRUE)
    
    # Changing format of Date
    LibraryRequestData$Request.Date <- as.Date(LibraryRequestData$Request.Date, origin = "1899-12-30")
    
    # Recording month of Request
    LibraryRequestData$RequestMonth <- strftime(LibraryRequestData$Request.Date, "%B")
    
    # Subset of data
    SubsetRequestData <- select(LibraryRequestData, c(Title, "#.of.requests", "Request.Date"))
    return (SubsetRequestData)
}

MonthlyFilter <- function(RequestData){
    FinalData <- select(RequestData, c(Title, "#.of.requests", "Request.Date"))
    FinalData <- rename(FinalData, Requests = "#.of.requests", RequestDate = "Request.Date")
    
    GroupedData <- FinalData %>% group_by(RequestDate) %>% summarise(Requests = sum(Requests))
    return (GroupedData)
}

Req <- ETLRequest()
Req <- MonthlyFilter(Req)
Req <- drop_na(Req)
Requests <- ts(Req[,2])
library(lubridate)
library(dummies)
Req$Season <- wday(Req$RequestDate, label=TRUE)
ReqNew <- as.data.frame(Req)
ReqNew <- dummy.data.frame(ReqNew, names = c("Season"), sep = "_")
arima.model <- arima(Requests, order = c(6,0,2))
arima.model.predict <- predict(arima.model, n.ahead = 28)

# Define server logic required to draw a histogram
shinyServer(function(input, output) {

    output$TSPlot <- renderPlot({

        
        library(ggplot2)
        ggplot(Req, aes(x=RequestDate, y=Requests)) +
            geom_point() + 
            geom_line(color = "#00AFBB") +
            xlab("Time (in Days)")

    })
    
    output$MAPlot <- renderPlot({
        
        library(zoo)
        
        temp.zoo<-zoo(Req$Requests,Req$RequestDate)
        
        ?rollmean
        m.av<-rollmean(temp.zoo, 3, fill = list(NA, NULL, NA))
        
        Req$amb.av=coredata(m.av)
        
        library(ggplot2)
        ggplot(Req, aes(x=RequestDate, y=Requests)) +
            geom_point() + 
            geom_line(color = "#00AFBB") +
            geom_line(aes(Req$RequestDate,Req$amb.av),color="red") +
            xlab("Time (in Days)")
    })
    
    output$LMPlot <- renderPlot({
        ggplot(Req, aes(x=RequestDate, y=Requests)) +
            geom_point() + 
            geom_line(color = "#00AFBB") +
            geom_smooth(method='lm') +
            xlab("Time (in Days)")
    })

    output$SmoothPlot <- renderPlot({
        ggplot(Req, aes(x=RequestDate, y=Requests)) +
            geom_point() + 
            geom_line(color = "#00AFBB") +
            geom_smooth(method='loess') +
            xlab("Time (in Days)")
    })
    
    output$LTSPlot <- renderPlot({
        
        ReqNew$P <- predict(lm(ReqNew$Requests ~ ReqNew$RequestDate * ReqNew$Season_Sun * ReqNew$Season_Mon 
                               * ReqNew$Season_Tue * ReqNew$Season_Wed 
                               * ReqNew$Season_Thu * ReqNew$Season_Fri * ReqNew$Season_Sat))
        
        ggplot() +
            geom_point(ReqNew, mapping = aes(x=RequestDate, y=Requests)) + 
            geom_line(ReqNew, mapping = aes(x=RequestDate, y=Requests), color = "blue") + 
            geom_line(ReqNew, mapping = aes(x=RequestDate, y=P),color = "red")
    })
    
    output$QTSPlot <- renderPlot({
        ReqNew$Q <- predict(lm(ReqNew$Requests ~ poly(Req$RequestDate,2) + ReqNew$Season_Sun + ReqNew$Season_Mon 
                               + ReqNew$Season_Tue + ReqNew$Season_Wed 
                               + ReqNew$Season_Thu + ReqNew$Season_Fri + ReqNew$Season_Sat))
        
        ggplot() +
            geom_point(ReqNew, mapping = aes(x=RequestDate, y=Requests)) + 
            geom_line(ReqNew, mapping = aes(x=RequestDate, y=Requests), color = "blue") + 
            geom_line(ReqNew, mapping = aes(x=RequestDate, y=Q),color = "red")
    })
    
    output$DiffPLot <- renderPlot({
        plot(diff(Requests))
        abline(a=0 ,b=0)
    })
    
    output$ADFTest <- renderPrint({
        return (adf.test(Requests, alternative = "stationary"))
    })
    
    output$ACFPlot <- renderPlot({
        acf(Requests)
    })
    
    output$PACFPlot <- renderPlot({
        pacf(Requests)
    })
    
    output$Arima <- renderPrint({
        return (arima.model)
    })
    
    output$ArimaPlot <- renderPlot({
        plot(Requests, xlab = "Time (Days)", xlim = c(0, 400)) 
        lines(arima.model.predict$pred, col ="red") 
        lines(arima.model.predict$pred+1.96*arima.model.predict$se, col=4, lty=2) 
        lines(arima.model.predict$pred-1.96*arima.model.predict$se, col=4, lty=2) 
    })
})
