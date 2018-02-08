#TESTING CHANGES ON GIT
# more testing
setwd("C:/Users/v-eritho/Desktop/RScripts/capstone_project/data/")
# lets test here

# more testing

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
library(cldr)
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
load("sample_us_corpus.RData")
load("training_us_corpus.RData")
load("testing_us_corpus.RData")

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
two_temp <- two_temp[1:50, ]

x <- NULL
y <- NULL
z <- NULL
two_grams_tidy <- NULL

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
        arrange(desc(count), .by_group = TRUE) %>%
        slice(1:5) %>%
        ungroup 
two_grams_tidy <- as.data.table(two_grams_tidy_df2)
save(two_grams_tidy, file = "two_grams_tidy_3000.RData")

# Make 3-grams frequency table
three_temp <- tokens_ngrams(training_us_tokens, n = 3L, skip = 0L, concatenator = "_")
three_temp <- as.data.table(table(unlist(three_temp)))
three_temp <- three_temp[order(N, decreasing = TRUE)]
save(three_temp, file = "three_temp.RData")
three_temp <- three_temp[1:3000, ]

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
        arrange(desc(count), .by_group = TRUE) %>%
        slice(1:5) %>%
        ungroup 
three_grams_tidy <- as.data.table(three_grams_tidy_df2)
save(three_grams_tidy, file = "three_grams_tidy_3000.RData")

# Make 4-grams frequency table
four_temp <- tokens_ngrams(training_us_tokens, n = 4L, skip = 0L, concatenator = "_")
four_temp <- as.data.table(table(unlist(four_temp)))
four_temp <- four_temp[order(N, decreasing = TRUE)]
save(four_temp, file = "four_temp.RData")
four_temp <- four_temp[1:3000, ]

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
        arrange(desc(count), .by_group = TRUE) %>%
        slice(1:5) %>%
        ungroup 
four_grams_tidy <- as.data.table(four_grams_tidy_df2)
save(four_grams_tidy, file = "four_grams_tidy_3000.RData")

# Make 5-grams frequency table
five_temp <- tokens_ngrams(training_us_tokens, n = 5L, skip = 0L, concatenator = "_")
five_temp <- as.data.table(table(unlist(five_temp)))
head(five_temp)
five_temp <- five_temp[order(N, decreasing = TRUE)]
save(five_temp, file = "five_temp.RData")
five_temp <- five_temp[1:3000, ]

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
        arrange(desc(count), .by_group = TRUE) %>%
        slice(1:5) %>%
        ungroup 
five_grams_tidy <- as.data.table(five_grams_tidy_df2)
save(five_grams_tidy, file = "five_grams_tidy_3000.RData")


# Make 6-grams frequency table
six_temp <- tokens_ngrams(training_us_tokens, n = 6L, skip = 0L, concatenator = "_")
six_temp <- as.data.table(table(unlist(six_temp)))
six_temp <- six_temp[order(N, decreasing = TRUE)]
save(six_temp, file = "six_temp.RData")
six_temp <- six_temp[1:3000, ]

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
        arrange(desc(count), .by_group = TRUE) %>%
        slice(1:5) %>%
        ungroup 
six_grams_tidy <- as.data.table(six_grams_tidy_df2)
save(six_grams_tidy, file = "six_grams_tidy_3000.RData")

# Model Input dataset
input_training <- rbindlist(l = list(two_grams_tidy,
                                three_grams_tidy,
                                four_grams_tidy,
                                five_grams_tidy,
                                six_grams_tidy),
                                idcol = TRUE)

input_training_3000 <- rbindlist(l = list(two_grams_tidy,
                                          three_grams_tidy,
                                          four_grams_tidy,
                                          five_grams_tidy,
                                          six_grams_tidy),
                                 idcol = TRUE)
save(input_training_3000, file = "input_training_3000.RData")

View(head(input_training, 5000))

f("I'd live and I'd")
# she cried ... for
# she cried for ... a
# she cried for a ... character(0)

## Skipgrams
# Currently this is slow when you have more than 6 or 7 words in the input
# because it is making all possible skip-gr,as/ so try to limit that.


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
                        return(input_training[`n-1` == toks_list[i], n])
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






# Backoff model (double backoff)
a <- NULL
f <- function(a = NULL) {
        b <- list(tokens(a)[[1]])
        b <- stri_join_list(b, sep = "_", collapse = TRUE)
        if (nrow(input[`n-1` == b, ]) != 0) {
                c <- as.character(b)
                c <- gsub(" ", "_", c)
                print(input[`n-1` == c, ])
                print("A")
        }
        if (nrow(input[`n-1` == b, ]) == 0) {
                c <- list(tokens(a)[[1]][-1])
                c <- stri_join_list(c, sep = "_", collapse = TRUE)
                if (nrow(input[`n-1` == c, ]) != 0) {
                        d <- as.character(c)
                        d <- gsub(" ", "_", d)
                        print(input[`n-1` == d, ])
                        print("B")
                }
                if (nrow(input[`n-1` == c, ]) == 0) {
                        d <- list(tokens(a)[[1]][-c(1, 2)])
                        d <- stri_join_list(d, sep = "_", collapse = TRUE)
                        if (nrow(input[`n-1` == d, ]) != 0) {
                                e <- as.character(d)
                                e <- gsub(" ", "_", e)
                                print(input[`n-1` == e, ])
                                print("C")
                        }
                        if (nrow(input[`n-1` == d, ]) == 0) {
                                e <- list(tokens(a)[[1]][-c(1, 2, 3)])
                                e <- stri_join_list(e, sep = "_", collapse = TRUE)
                                print(input[`n-1` == e, ])
                                print("D")
                        }
                }
        }       
}

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
readLines(con, 5)   # Read the first 5 lines
close(con)    # Close the connection

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


###############################################################################








#Creating a dictionary
#myDict <- dictionary(list(terror = c("terrorism", "terrorists", "threat"),
#                          economy = c("jobs", "business", "grow", "work")))
#byPresMat <- dfm(recentCorpus, dictionary = myDict)
#byPresMat

# Similarties between texts
#presDfm <- dfm(corpus_subset(data_corpus_inaugural, Year > 1980), 
#               remove = stopwords("english"), stem = TRUE, remove_punct = TRUE)
#obamaSimil <- textstat_simil(presDfm, c("2009-Obama" , "2013-Obama"), 
#                             margin = "documents", method = "cosine")

# Dendrograms
# presDfm <- dfm(corpus_subset(data_corpus_SOTU, Date > as.Date("1980-01-01")), 
#               stem = TRUE, remove_punct = TRUE,
#               remove = stopwords("english"))
#presDfm <- dfm_trim(presDfm, min_count = 5, min_docfreq = 3)
## hierarchical clustering - get distances on normalized dfm
#presDistMat <- textstat_dist(dfm_weight(presDfm, "relfreq"))
## hierarchical clustering the distance object
#presCluster <- hclust(presDistMat)
## label with document names
#presCluster$labels <- docnames(presDfm)
## plot as a dendrogram
#plot(presCluster, xlab = "", sub = "", main = "Euclidean Distance on Normalized Token Frequency")

#Topic model
# trimmed_sample_us_DFM <- dfm_trim(sample_us_DFM, min_count = 5, 
#                                  # max_docfreq = 10, 
#                                  verbose = TRUE)
#trimmed_sample_us_DFM

#if (require(topicmodels)) {
#        myLDAfit20 <- LDA(convert(trimmed_sample_us_DFM, to = "topicmodels"), k = 20)
#        get_terms(myLDAfit20, 5)
#}





# Old way I was making n-gram tables; VERY slow, took like 8 hours for only 5% sample

# Top 2-grams
table2 <- textstat_collocations(us_corpus, size = 2)
table2 <- table2[order(table2$count, decreasing = TRUE), ]
head(table2)

# Top 3-grams
table3 <- textstat_collocations(us_corpus, size = 3)
table3 <- table3[order(table3$count, decreasing = TRUE), ]
head(table3)

# Top 4-grams
table4 <- textstat_collocations(us_corpus, size = 4)
table4 <- table4[order(table4$count, decreasing = TRUE), ]

# Top 5-grams
table5 <- textstat_collocations(us_corpus, size = 5)
table5 <- table5[order(table5$count, decreasing = TRUE), ]
head(table5)

# CREATE DATASET WITH 3 COLUMNS: 
# 1) n-1 words in the n-gram, 
# 2) a prediction that is the last word, 
# 3) and a count variable for the frequency of occurrence of this n-gram (ordered)

# Two-grams
temp2 <- data.table()
temp3 <- NULL

for (i in 1:length(table2$collocation[1:50])) {
        temp2 <- rbind(temp2, strsplit(table3$collocation[[i]][1:50], " ")[[1]][2])
        temp3 <- rbind(temp3, strsplit(table3$collocation[[i]][1:50], " ")[[1]][1])
}

two_grams <- data.table(temp3, temp2, table2$count[1:50])
two_grams <- two_grams[!duplicated(two_grams[, 1], fromLast = FALSE), ]
two_grams

# Three-grams
temp2 <- data.table()
temp3 <- rbind(c(NULL, NULL))

for (i in 1:length(table3$collocation[1:50])) {
        temp2 <- rbind(temp2, strsplit(table3$collocation[[i]][1:50], " ")[[1]][3])
        temp3 <- rbind(temp3, strsplit(table3$collocation[[i]][1:50], " ")[[1]][1:2])
}

three_grams <- data.table(temp3, temp2, table3$count[1:50])
three_grams <- three_grams[!duplicated(three_grams[, 1:2], fromLast = FALSE), ]
three_grams

# Four-grams
temp2 <- data.table()
temp3 <- rbind(c(NULL, NULL, NULL))

for (i in 1:length(table4$collocation[1:50])) {
        temp2 <- rbind(temp2, strsplit(table4$collocation[[i]][1:50], " ")[[1]][4])
        temp3 <- rbind(temp3, strsplit(table4$collocation[[i]][1:50], " ")[[1]][1:3])
}

four_grams <- data.table(temp3, temp2, table4$count[1:50])
four_grams <- four_grams[!duplicated(four_grams[, 1:3], fromLast = FALSE), ]
four_grams

# Five-grams
temp2 <- data.table()
temp3 <- rbind(c(NULL, NULL, NULL, NULL))

for (i in 1:length(table5$collocation[1:50])) {
        temp2 <- rbind(temp2, strsplit(table5$collocation[[i]][1:50], " ")[[1]][5])
        temp3 <- rbind(temp3, strsplit(table5$collocation[[i]][1:50], " ")[[1]][1:4])
}

five_grams <- data.table(temp3, temp2, table5$count[1:50])
five_grams <- five_grams[!duplicated(five_grams[, 1:4], fromLast = FALSE), ]
five_grams


two_grams
three_grams
four_grams
five_grams


#QUIZ
load("four_grams.RData")
zzz <- head(four_grams[grep("^you_must_be", four_grams$V1), ], 100)
head(zzz)

#*********************************************
# Take four_grams table (zzz) into a 5, columns: 3 input, 1 prediction, and one count
x <- NULL
y <- NULL
z <- NULL
four_gram_tidy <- NULL

for (i in 1:nrow(four_grams[1:10000, 1])){
        z <- list(strsplit(as.character(four_grams[i, 1]), split = "_")[[1]][1:3])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(four_grams[1:10000, 1])){
        z <- unlist(strsplit(as.character(four_grams[i, 1]), split = "_"))[4]
        y <- rbind(y, z)
}

table1 <- data.table(x, y, four_grams[1:10000, ]$N)
names(table1) <- c("n-1", "n", "count")
four_gram_tidy <- rbind(four_gram_tidy, table1)
four_grams <- four_grams[!duplicated(four_grams[, 1]), ]
head(four_gram_tidy)
#*********************************************
z <- list(strsplit(as.character(four_grams[i, 1]), split = "_")[[1]][1:3])
z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
z

list(strsplit(as.character(zzz[i, 1]), split = "_")[[1]][1:3])

str(z)
strsplit(as.character(zzz[i, 1]), split = "_")[[1]][1:3]
stri_join_list(z, sep = " ", collapse = TRUE)

stri_join_list(stri_extract_all_words(c("Lorem ipsum dolor sit amet.",
                                        "You're gonna get away with this.")), sep=", ")

input <- "i love to"
install.packages("stringi")
library(stringi)

stri_join_list(strsplit(as.character(input), split = " "), sep = " ", collapse = TRUE)

strsplit(as.character(input), split = " ")


stri_paste(letters, collapse='')



z <- unlist(strsplit(as.character(zzz[i, 1]), split = "_"))[1:3]
z <- as.character(z)

str(z)


as.character(zzz[3, 1])
unlist(strsplit(as.character(zzz[i, 1]), split = "_"))[4]
a <- strsplit(as.character(zzz[1, 1]), "_")[4]
a
zzz[1, 1]
length(zzz[, 1])
unlist(strsplit(as.character(zzz[1, 1]), split = "_"))[4]

str(zzz)









