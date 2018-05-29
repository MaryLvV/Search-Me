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
  titlePanel("What am I searching for?"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       sliderInput("date",
                   "Month and year:",
                   min = 1,
                   max = 50,
                   value = 30)
    ),
    
    # Show the wordcloud
    mainPanel(
       plotOutput("cloudPlot")
    )
  )
))
