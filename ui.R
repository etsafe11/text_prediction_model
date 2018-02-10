library(shiny)
library(quanteda)
#load("input_training.RData")

fluidPage(
        textInput("txt", "Enter text"),
        verbatimTextOutput("m1pred", placeholder = TRUE)
)
