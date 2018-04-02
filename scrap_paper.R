# Building an n-gram text prediction model from scratch.

setwd("C:/Users/v-eritho/Desktop/text_prediction_model")

library(dplyr)
library(readr)
library(rlang)
library(mnormt)
library(tidytext)
library(quanteda)
library(readtext)
library(topicmodels)
library(ggplot2)
library(qdapDictionaries)
library(qdapRegex)
library(qdapTools)
library(RColorBrewer)
library(NLP)
library(tm)
library(SnowballC)
library(slam)
library(sqldf)
library(data.table)
library(stringi)
library(shiny)

# The below data.table objects are inputs to our model.
# To save many hours of processing time, load them from local directory.
# load("two_grams_tidy_500000.RData")
# load("three_grams_tidy_400000.RData")
# load("four_grams_tidy_300000.RData")
# load("five_grams_tidy_300000.RData")
# load("six_grams_tidy_300000.RData")
load("input_training_750000.RData")

#us_blogs <- readtext("en_US.blogs.txt", text_field = 1)
us_blogs <- read_lines("en_US.blogs.txt")
us_news <- read_lines("en_US.news.txt")
us_twitter <- read_lines("en_US.twitter.txt")

# write.csv(sample_us_blogs, file = "sample_us_blogs.csv")
# read.csv("sample_us_blogs.xls", header = FALSE)

# Convert our dataframe to a corpus object
us_blogs_corpus <- corpus(us_blogs)
us_news_corpus <- corpus(us_news)
us_twitter_corpus <- corpus(us_twitter)

# Add document-level variables (docvars) for blogs, news and twitter
docvars(us_blogs_corpus, field = "source") <- rep("blogs", times = nrow(us_blogs_corpus$documents))
docvars(us_news_corpus, field = "source") <- rep("news", times = nrow(us_news_corpus$documents))
docvars(us_twitter_corpus, field = "source") <- rep("twitter", times = nrow(us_twitter_corpus$documents))

# Merge the three data sources into a single corpus
us_corpus <- us_blogs_corpus + us_news_corpus + us_twitter_corpus

# 35% sample
set.seed(555)
sample_us_corpus <- corpus_sample(us_corpus, 
                                  size = round(.35 * nrow(us_corpus$documents)))

training_us_corpus <- corpus_subset(sample_us_corpus, 
                                    1:ndoc(sample_us_corpus) %in% 1:1000000)

testing_us_corpus <- corpus_subset(sample_us_corpus, 
                                   1:ndoc(sample_us_corpus) %in% 1000001:1300000)

validation_us_corpus <- corpus_subset(sample_us_corpus, 
                                      1:ndoc(sample_us_corpus) %in% 1300001:1450000)

# Tokenize the corpus, removing punctuation etc
training_us_tokens <- tokens(training_us_corpus, what = "word", 
                             remove_numbers = TRUE, remove_punct = TRUE,
                             remove_symbols = TRUE, remove_separators = TRUE,
                             remove_twitter = TRUE, remove_hyphens = TRUE, 
                             remove_url = TRUE, ngrams = 1L, skip = 0L, concatenator = "_")

# Make 2-grams frequency table
two_temp <- tokens_ngrams(training_us_tokens, n = 2L, skip = 0L, concatenator = "_")
two_temp <- as.data.table(table(unlist(two_temp)))
two_temp <- two_temp[order(N, decreasing = TRUE)]
save(two_temp, file = "two_temp.RData")
two_temp <- two_temp[1:750000, ]
str(two_temp)
x <- NULL
y <- NULL
z <- NULL

for (i in 1:nrow(two_temp[, 1])) {
        z <- list(strsplit(as.character(two_temp[i, 1]), split = "_")[[1]][1])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(two_temp[, 1])) {
        z <- unlist(strsplit(as.character(two_temp[i, 1]), split = "_"))[2]
        y <- rbind(y, z)
}

two_grams_tidy <- data.table(x, y, two_temp$N)
names(two_grams_tidy) <- c("n-1", "n", "count")

two_grams_tidy_df1 <-as.data.frame(two_grams_tidy)

two_grams_tidy_df2 <- two_grams_tidy_df1 %>%
        group_by(`n-1`) %>%
        arrange(`n-1`, desc(count)) %>%
        slice(1:5) %>%
        ungroup 

two_grams_tidy <- as.data.table(two_grams_tidy_df2)
save(two_grams_tidy, file = "two_grams_tidy_750000.RData")

# Make 3-grams frequency table
three_temp <- tokens_ngrams(training_us_tokens, n = 3L, skip = 0L, concatenator = "_")
three_temp <- as.data.table(table(unlist(three_temp)))
three_temp <- three_temp[order(N, decreasing = TRUE)]
save(three_temp, file = "three_temp.RData")
three_temp <- three_temp[1:750000, ]

x <- NULL
y <- NULL
z <- NULL
three_grams_tidy <- NULL

for (i in 1:nrow(three_temp[, 1])) {
        z <- list(strsplit(as.character(three_temp[i, 1]), split = "_")[[1]][1:2])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(three_temp[, 1])) {
        z <- unlist(strsplit(as.character(three_temp[i, 1]), split = "_"))[3]
        y <- rbind(y, z)
}

three_grams_tidy <- data.table(x, y, three_temp$N)
names(three_grams_tidy) <- c("n-1", "n", "count")

three_grams_tidy_df1 <-as.data.frame(three_grams_tidy)

three_grams_tidy_df2 <- three_grams_tidy_df1 %>%
        group_by(`n-1`) %>%
        arrange(`n-1`, desc(count)) %>%
        slice(1:5) %>%
        ungroup 

three_grams_tidy <- as.data.table(three_grams_tidy_df2)
save(three_grams_tidy, file = "three_grams_tidy_750000.RData")

# Make 4-grams frequency table
four_temp <- tokens_ngrams(training_us_tokens, n = 4L, skip = 0L, concatenator = "_")
four_temp <- as.data.table(table(unlist(four_temp)))
four_temp <- four_temp[order(N, decreasing = TRUE)]
save(four_temp, file = "four_temp.RData")
four_temp <- four_temp[1:750000, ]

x <- NULL
y <- NULL
z <- NULL
four_grams_tidy <- NULL

for (i in 1:nrow(four_temp[, 1])) {
        z <- list(strsplit(as.character(four_temp[i, 1]), split = "_")[[1]][1:3])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(four_temp[, 1])) {
        z <- unlist(strsplit(as.character(four_temp[i, 1]), split = "_"))[4]
        y <- rbind(y, z)
}

four_grams_tidy <- data.table(x, y, four_temp$N)
names(four_grams_tidy) <- c("n-1", "n", "count")

four_grams_tidy_df1 <-as.data.frame(four_grams_tidy)

four_grams_tidy_df2 <- four_grams_tidy_df1 %>%
        group_by(`n-1`) %>%
        arrange(`n-1`, desc(count)) %>%
        slice(1:5) %>%
        ungroup 

four_grams_tidy <- as.data.table(four_grams_tidy_df2)
save(four_grams_tidy, file = "four_grams_tidy_750000.RData")

# Make 5-grams frequency table
five_temp <- tokens_ngrams(training_us_tokens, n = 5L, skip = 0L, concatenator = "_")
five_temp <- as.data.table(table(unlist(five_temp)))
five_temp <- five_temp[order(N, decreasing = TRUE)]
save(five_temp, file = "five_temp.RData")
five_temp <- five_temp[1:750000, ]

x <- NULL
y <- NULL
z <- NULL
five_grams_tidy <- NULL

for (i in 1:nrow(five_temp[, 1])) {
        z <- list(strsplit(as.character(five_temp[i, 1]), split = "_")[[1]][1:4])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(five_temp[, 1])) {
        z <- unlist(strsplit(as.character(five_temp[i, 1]), split = "_"))[5]
        y <- rbind(y, z)
}

five_grams_tidy <- data.table(x, y, five_temp$N)
names(five_grams_tidy) <- c("n-1", "n", "count")

five_grams_tidy_df1 <-as.data.frame(five_grams_tidy)

five_grams_tidy_df2 <- five_grams_tidy_df1 %>%
        group_by(`n-1`) %>%
        arrange(`n-1`, desc(count)) %>%
        slice(1:5) %>%
        ungroup 

five_grams_tidy <- as.data.table(five_grams_tidy_df2)
save(five_grams_tidy, file = "five_grams_tidy_750000.RData")


# Make 6-grams frequency table
six_temp <- tokens_ngrams(training_us_tokens, n = 6L, skip = 0L, concatenator = "_")
six_temp <- as.data.table(table(unlist(six_temp)))
six_temp <- six_temp[order(N, decreasing = TRUE)]
save(six_temp, file = "six_temp.RData")
six_temp <- six_temp[1:750000, ]

x <- NULL
y <- NULL
z <- NULL
six_grams_tidy <- NULL

for (i in 1:nrow(six_temp[, 1])) {
        z <- list(strsplit(as.character(six_temp[i, 1]), split = "_")[[1]][1:5])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(six_temp[, 1])) {
        z <- unlist(strsplit(as.character(six_temp[i, 1]), split = "_"))[6]
        y <- rbind(y, z)
}

six_grams_tidy <- data.table(x, y, six_temp$N)
names(six_grams_tidy) <- c("n-1", "n", "count")

six_grams_tidy_df1 <-as.data.frame(six_grams_tidy)

six_grams_tidy_df2 <- six_grams_tidy_df1 %>%
        group_by(`n-1`) %>%
        arrange(`n-1`, desc(count)) %>%
        slice(1:5) %>%
        ungroup 

six_grams_tidy <- as.data.table(six_grams_tidy_df2)
save(six_grams_tidy, file = "six_grams_tidy_750000.RData")


# Model Input dataset
input_training <- rbindlist(l = list(two_grams_tidy,
                                three_grams_tidy,
                                four_grams_tidy,
                                five_grams_tidy,
                                six_grams_tidy),
                                idcol = TRUE)

save(input_training, file = "input_training_750000.RData")

View(head(input_training, 5000))

f("I'd live and I'd")

# Model for returning FIVE results 
a <- NULL
f <- function(a = NULL) {
        toks <- tokens(a)
        toks_list <- tokens_skipgrams(toks,
                                      n = 1:length(toks[[1]]),
                                      skip = 0:4,
                                      concatenator = "_")
        toks_list <- rev(toks_list[[1]])
        last <- toks[[1]][length(toks[[1]])]
        toks_list <- toks_list[grep(last, toks_list)]
        for (i in 1:length(toks_list)) {
                if (nrow(input_training[`n-1` == toks_list[i], ]) != 0) {
                        x <- c(x, input_training[`n-1` == toks_list[i], n])
                        return(c("Most likely next words:\n", 
                                 x[1], "\n", 
                                 (if(is.na(x[2]) == TRUE) { "" } else {x[2]}), "\n", 
                                 (if(is.na(x[3]) == TRUE) { "" } else {x[3]}), "\n", 
                                 (if(is.na(x[4]) == TRUE) { "" } else {x[4]}), "\n", 
                                 (if(is.na(x[5]) == TRUE) { "" } else {x[5]}), "\n"))
                }
        }
}

f("I need to")

str(f("I need to"))

# Model for returning a SINGLE result 
a <- NULL
f <- function(a = NULL) {
        toks <- tokens(a)
        toks_list <- tokens_skipgrams(toks,
                                      n = 1:length(toks[[1]]),
                                      skip = 0:4,
                                      concatenator = "_")
        toks_list <- rev(toks_list[[1]])
        last <- toks[[1]][length(toks[[1]])]
        toks_list <- toks_list[grep(last, toks_list)]
        for (i in 1:length(toks_list)) {
                if (nrow(input[`n-1` == toks_list[i], ]) != 0) {
                        return(input_trainng[`n-1` == toks_list[i], n])
                        break()
                }
        }
}
f("I never thought I'd")





# table summary of our different data type
table(us_corpus$documents$source)

# Create sample corpus which is 10% of the original.
# We use this sample corpus going forward.
sample_us_corpus <- corpus_sample(us_corpus, size = round(.10 * nrow(us_corpus$documents)))
save(sample_us_corpus, file = "sample_us_corpus.RData")

# See how many of each source type made the sample
table(sample_us_corpus$documents$source) 
#knitr::kable(sample_us_corpus$documents$source, caption = "Sample Corpus") 

# Shows each line of the corpus with # of Types, Tokens and Sentences
summary(sample_us_corpus)

#Pull the 7th line in the blogs document and confirm it has 6 tokens
texts(sample_us_corpus)[7]

#Pull the tokens from the 7th line
tokens(sample_us_corpus[7])

# We can consider tokens to be individual characters rather than words if we want
# tokens(us_blogs_corpus[7], what = "character")

# Keywords-in-Context (kwic) performs a search for a word and allows us to view the contexts
kwic(sample_us_corpus[1:50000], "terror")
str(us_corpus)

# Top 1-grams
table1 <- topfeatures(us_DFM, n = 50, decreasing = TRUE, scheme = "count")
us_DFM@Dim[2]
# 925,854 types in us_DFM (before filtering anything out)
head(attr(table1, "names"), 50) # list of just the tokens themselves (they're attributes)
save(table1, file = "table1.RData")





length(tokens(sample_us_corpus$documents$texts[1:5],
              remove_numbers = TRUE, 
              remove_punct = TRUE,
              remove_symbols = TRUE, 
              remove_separators = TRUE,
              remove_twitter = TRUE, 
              remove_hyphens = TRUE, 
              remove_url = TRUE))


a <- tokens("I love partying so much. It is awesome.", 
            remove_numbers = TRUE, 
            remove_punct = TRUE,
            remove_symbols = TRUE,
            remove_separators = TRUE,
            remove_url = TRUE)
a
tokens_ngrams(a, n = 3L, 
              #skip = 1,
              concatenator = "_")





kable(us_corpus$documents$source, caption = "Merged Corpus")

browseVignettes("data.table")
library(knitr)
# EXPLORATORY ANALYSIS BELOW (bulk of it)
kable(sample_us_corpus$documents$source, caption = "Sample Corpus") 
# How many unique words do you need in a frequency sorted 
# dictionary to cover 50% of all word instances in the language? 90%?
sum(textstat_frequency(us_DFM)$frequency)
# There are 101,627,352 unique tokens in us_DFM per the above line.
# so we need to cover 50,813,676 words. Below code shows that 
# the top 153 types cover 50,813,676 tokens (50% of us_corpus):
sum(textstat_frequency(us_DFM)$frequency[1:153])
# It takes about 9,000 types to cover 90% of the corpus (91,464,617 types):
sum(textstat_frequency(us_DFM)$frequency[1:9000])

# Plot 30 most frequent words
library("ggplot2")
ggplot(textstat_frequency(us_DFM)[1:30, ], 
       aes(x = reorder(feature, frequency), y = frequency)) +
        geom_point() + 
        labs(x = NULL, y = "Frequency")

# Find the maximum length (# of charcters) in each line
max_char_blogs <- nchar(us_blogs[1])
for(i in 1:length(us_blogs)) {
        if(nchar(us_blogs[i]) > max_char_blogs) 
                max_char_blogs <- nchar(us_blogs[i])
}
max_char_blogs

max_char_news <- nchar(us_news[1])
for(i in 1:length(us_news)) {
        if(nchar(us_news[i]) > max_char_news) 
                max_char_news <- nchar(us_news[i])
}
max_char_news

max_char_twitter <- nchar(us_twitter[1])
for(i in 1:length(us_twitter)) {
        if(nchar(us_twitter[i]) > max_char_twitter) 
                max_char_twitter <- nchar(us_twitter[i])
}
max_char_twitter

# Ratio of "love" to "hate" in Twitter dataset
length(grep("Trump", us_twitter)) / length(grep("Clinton", us_twitter)) 

# Find all tweets w/ "biostats" in them (only one result!)
us_twitter[grep("biostats", us_twitter)]

# Identify the observations with a funny long string
grep("I love partying", us_twitter)

# Wordcloud. Note we cap the # of rows at 10,000 for performance reasons
# First, create and view document-frequency matrix (DFM) removing punctuation
# Can also remove stopwords or other specified words if we like using remove parameter
us_DFM <- dfm(us_corpus, 
              verbose = TRUE, 
              include_docvars = TRUE,
              #remove = stopwords("english"), - can also add my own custom words to ignore using remove
              #stem = TRUE - I likely will want to set this to TRUE
              remove_punct = TRUE)
save(us_DFM, file = "us_DFM.RData")

sample_us_DFM <- dfm(sample_us_corpus, 
                     verbose = TRUE, 
                     include_docvars = TRUE,
                     #remove = stopwords("english"), - can also add my own custom words to ignore using remove
                     #stem = TRUE - I likely will want to set this to TRUE
                     remove_punct = TRUE)
save(sample_us_DFM, file = "sample_us_DFM.RData")

set.seed(555)
textplot_wordcloud(sample_us_DFM[1:500], min.freq = 6, random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))

# See the source of the first few rows of our sampled dataset
# DO I WANT to remove several types such as "n" or "de" or names like "david"??
head(docvars(sample_us_DFM))

# View a list of the top stopwords, in case you want to strip them out
head(stopwords("english"), 20)

# list of the most frequently occurring features (top 50,000 rows)
topfeatures(sample_us_DFM, 1000)

# Create ngrams and skipgrams from tokens
# NEED TO FIX THIS PART
str(sample_us_corpus$documents$texts)
toks <- tokens(sample_us_corpus$documents$texts)
tokens_ngrams(toks, n = 2)

# How to read line-by-line... VERY IMPORTANT to close connection.
con <- file("en_US.blogs.txt", "r") 
readLines(con, 5)# Read the first 5 lines
close(con) # Close the connection

# Tidytext package
# Word frequency table... using SAMPLE dataset to improve computation times
us_blogs_tokens <- as_tibble(sample_us_blogs) %>% 
        unnest_tokens(word, value, token = "ngrams", n = 1)
us_blogs_tokens %>% count(word, sort = T) %>% print(n=35)

us_news_tokens <- as_tibble(sample_us_news) %>% 
        unnest_tokens(word, value, token = "ngrams", n = 1)
us_news_tokens %>% count(word, sort = T) %>% print(n=35)

us_twitter_tokens <- as_tibble(sample_us_twitter) %>% 
        unnest_tokens(word, value, token = "ngrams", n = 1)
us_twitter_tokens %>% count(word, sort = T) %>% print(n=35)

# Language detection
# This exercise requires installing an old version of "cldr" package using:
# devtools::install_version("cldr",version="1.1.0")
# The below two lines tell us how many of the Types are non-English.
# This is different from how many Tokens are non-English of course.
allTypes <- attr(topfeatures(us_DFM, n = 999999999), "names")
table(detectLanguage(allTypes)$detectedLanguage)

# Babble
install.packages("ngram")
library(ngram)
str <- "I like to go to the dog park"
ng <- ngram(str)
babble(ng, genlen=2)
get.nextwords(ng)

