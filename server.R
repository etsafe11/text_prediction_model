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
                                return(c("Most likely next words:\n", 
                                         x[1], "\n", x[2], "\n", x[3], "\n", x[4], "\n", x[5]))
                        }
                }
        })
}
