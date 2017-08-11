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
  titlePanel("Predict the next word"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
       textInput("user_words",
                   "Type yout text:",
                 value = "My text"),
       sliderInput("number_of_words",
                 "Number of Words:",
                 min = 1,
                 max = 10,
                 value = 3),
       actionButton("submit_text", label = "Suggest words")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h1("Predicted Words:"),
      tableOutput("predictedWords")
    )
  )
))
