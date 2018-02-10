library(shiny)
library(quanteda)
load("input_training_750000.RData")

fluidPage(
        titlePanel("n-gram Text Prediction Model"),
        textInput("txt", "Enter text:"),
        verbatimTextOutput("m1pred", placeholder = TRUE)
)
