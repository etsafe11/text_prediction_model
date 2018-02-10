library(shiny)
library(quanteda)
#load("input_training.RData")

function(input, output) {
        output$m1pred <- renderText({ 
                validate(
                        need(input$txt, "Please enter text"))
                toks <- tokens(input$txt)
                toks <- tail(unlist(toks), 8)
                toks_list <- tokens_skipgrams(toks,
                                              n = 1:length(toks[[1]]),
                                              skip = 0:4,
                                              concatenator = "_")
                toks_list <- rev(toks_list)
                last <- toks[length(toks)]
                toks_list <- toks_list[grep(last, toks_list)]
                print(toks_list)
                x <- NULL
                for (i in 1:length(toks_list)) {
                        if (nrow(input_training[`n-1` == toks_list[i], ]) != 0) {
                                x <- c(x, input_training[`n-1` == toks_list[i], n])
                                return(c(x[1], " | ", x[2], " | ", x[3], " | ", x[4], " | ", x[5]))
                        }
                }
        })
}
