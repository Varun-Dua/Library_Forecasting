#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
shinyUI(fluidPage(

    # Application title
    titlePanel("Forecasting Library Requests"),

    sidebarLayout(
        sidebarPanel(
            radioButtons("radio1", h3("Variable:"),
                         choices = list("Requests" = 1
                                        ),selected = 1)
        ),

        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Time Series", plotOutput("TSPlot")),
                        tabPanel("Moving Averages Chart", plotOutput("MAPlot")),
                        tabPanel("Linear Trend", plotOutput("LMPlot")),
                        tabPanel("Smooth Trend", plotOutput("SmoothPlot")),
                        tabPanel("Linear Trend & Seasonality", plotOutput("LTSPlot")),
                        tabPanel("Quadratic Trend & Seasonality", plotOutput("QTSPlot")),
                        tabPanel("Detrending - Differenced Plot", plotOutput("DiffPLot")),
                        tabPanel("ADF Test", verbatimTextOutput("ADFTest")),
                        tabPanel("Autocorrelation - ACF", plotOutput("ACFPlot")),
                        tabPanel("Partial Correlation - PACF", plotOutput("PACFPlot")),
                        tabPanel("ARIMA Model", verbatimTextOutput("Arima")),
                        tabPanel("ARIMA Prediction", plotOutput("ArimaPlot")))
        )
    )
))
