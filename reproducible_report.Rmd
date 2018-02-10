---
title: "n-gram Text Prediction Model"
author: "Eric Thompson"
date: "January 2018"
output: html_document
---

```{r setup, include = FALSE}
knitr::opts_chunk$set(echo = TRUE)
knitr::opts_chunk$set(root.dir = 'C:/Users/v-eritho/Desktop/RScripts/capstone_project/data/')
```


### Introduction

This reproducible report demonstrates how to build a text prediction model beginning with text files
of several million blog posts, tweets and news articles.  We clean and tokenize the text and build an
ngram text prediction algorithm.  We have built a Shiny app as our user interface, in which the user 
enters any amount of text and then almost instantaneously receives a prediction for the next word. 

Here is the GitHub repo containing a README and full R scripts for our Shiny app: 
<https://github.com/etsafe11/text_prediction_model>

Our Shiny app is deployed here: <https://etsafe11.shinyapps.io/text_prediction_model_11/>

### Data Ingestion

We begin by ingesting three text files:

1. a set of text lines of blog posts
2. a set of text lines of  news articles
3. a set of text lines of Tweets

For modeling purposes we will soon merge the three types of text lines into a single 
corpus, but first we create document-level variables to classify each line as either 
blog, news or Twitter.

```{r include = FALSE, cache = FALSE, eval = TRUE}
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
library(knitr)

setwd("C:/Users/v-eritho/Desktop/RScripts/capstone_project/data/")

# You may need to right click the .RData file in the file folder and Open With RStudio
load("table1.RData")
load("table2.RData")
load("table3.RData")
load("table4.RData")
load("table5.RData")
load("us_DFM.RData")
load("sample_us_DFM.RData")
load("input_testing.RData")
load("input_training.RData")
```

```{r eval = TRUE, cache = TRUE, include = TRUE}
us_blogs <- read_lines("en_US.blogs.txt")
us_news <- read_lines("en_US.news.txt")
us_twitter <- read_lines("en_US.twitter.txt")

us_blogs_corpus <- corpus(us_blogs)
us_news_corpus <- corpus(us_news)
us_twitter_corpus <- corpus(us_twitter)

# Add document-level variables (docvars) for blogs, news and twitter
docvars(us_blogs_corpus, field = "source") <- rep("blogs", times = nrow(us_blogs_corpus$documents))
docvars(us_news_corpus, field = "source") <- rep("news", times = nrow(us_news_corpus$documents))
docvars(us_twitter_corpus, field = "source") <- rep("twitter", times = nrow(us_twitter_corpus$documents))
```


### Line Counts

Next we merge the three data sources into a single corpus and make a table showing
how many documents of each source type exist in our newly-merged corpus. We see there 
are about 900K blog lines, 1 million news lines and 2.4 million Tweets.

```{r include = TRUE, eval = FALSE}
us_corpus <- us_blogs_corpus + us_news_corpus + us_twitter_corpus
table(us_corpus$documents$source)
```


### Sample Dataset

Since our merged dataset is over 4 million text entries, we can retain a large sample 
size but improve processing time dramatically by using only 5% of the original data
set as the input.

Below we see how many text lines of each type (blogs, news, Twitter) exist in the 
sample dataset.  This will be helpful in constructing our text prediction model. 
Based on the line counts below, we see that the sample data set is indeed 
approximately five percent of the total, original data set.  Note that for most of 
the exploratory analysis below, we are using the original, merged dataset, rather 
than this sample dataset.

```{r include = TRUE, eval = FALSE}
# 35% sample
set.seed(555)
sample_us_corpus <- corpus_sample(us_corpus, 
                                  size = round(.35 * nrow(us_corpus$documents)))

training_us_corpus <- corpus_subset(sample_us_corpus, 
                                    1:ndoc(sample_us_corpus) %in% 1:1000000)

testing_us_corpus <- corpus_subset(sample_us_corpus, 
                                   1:ndoc(sample_us_corpus) %in% 1000001:1300000)
```


### Exploratory Analysis

#### Keywords-in-Context
Below is a quick way to see a particular word in its various contexts.

```{r include = TRUE, eval = FALSE, cache = TRUE}
kwic(sample_us_corpus[1:25000], "terror")
```


#### Word Counts

In addition to our line counts, we want to count words.  Here we see there are over 
101 million different words in our original dataset.  Below is a table showing the
most frequently-occuring word.  You can see that "the" appears over 4.7 million times.

```{r include = TRUE, eval = FALSE}
sum(textstat_frequency(us_DFM)$frequency)
head(textstat_frequency(us_DFM))
```

Next we consider the distribution of the word counts even futher.  Specifically, how 
many unique words does one need in a frequency-sorted dictionary to cover 50% of all 
word instances in the merged corpus?  Below we see that the top 153 most-common words 
represent about 50.8 million words, which is approximately 50% of our merged corpus.  

```{r include = TRUE, eval = FALSE}
sum(textstat_frequency(us_DFM)$frequency[1:153])
```

Similarly, to cover 90% of the word instances in the corpus, one needs the top 9,000 
most commonly-occurring words.

```{r include  = TRUE, eval = FALSE}
sum(textstat_frequency(us_DFM)$frequency[1:9000])
```

Next we plot the 30 most frequent words:

```{r include = TRUE, eval = FALSE}
library(ggplot2)
ggplot(textstat_frequency(us_DFM)[1:30, ], 
       aes(x = reorder(feature, frequency), y = frequency)) +
       geom_point() + 
       labs(x = NULL, y = "Frequency")
```

Next we find the maximum number of characters in each line.  As expected, the 
maximum length of any Tweet is 140 characters.

```{r include = TRUE, eval = TRUE}
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
```

We can easily calculate the ratio of the word "love" to "hate" in the Twitter dataset.

```{r}
length(grep("love", us_twitter)) / length(grep("hate", us_twitter)) 
```

We can create a wordcloud of 130 randomly-sampled elements of our merged corpus. 

```{r include = TRUE, eval = FALSE}
# Word Cloud
set.seed(555)
textplot_wordcloud(sample_us_DFM[1:130], 
                   min.freq = 6, 
                   random.order = FALSE,
                   rot.per = .25, 
                   colors = RColorBrewer::brewer.pal(8,"Dark2"))
```


#### n-grams

An n-gram is a contiguous sequence of n items from a given sequence of text or speech.
n-grams will be foundational to our model.  

We first show that our merged dataset contains 925,854 different words, or "types".

```{r include=TRUE, eval = FALSE}
us_DFM@Dim[2]
```

Below are the top 50 1-grams (single "types"):

```{r include = TRUE, eval = FALSE}
table1 <- topfeatures(us_DFM, n = 50, decreasing = TRUE, scheme = "count")
head(attr(table1,"names"), 50)
```

```{r include = TRUE, eval = FALSE}
# Top 2-grams
table2 <- textstat_collocations(sample_us_corpus, size = 2)
table2 <- table2[order(table2$count, decreasing = TRUE), ]
head(table2)
``` 

```{r include = TRUE, eval = FALSE}
# Top 3-grams
table3 <- textstat_collocations(sample_us_corpus, size = 3)
table3 <- table3[order(table3$count, decreasing = TRUE), ]
head(table3)
``` 

```{r include = TRUE, eval = FALSE}
# Top 3-grams
table4 <- textstat_collocations(sample_us_corpus, size = 4)
table4 <- table4[order(table4$count, decreasing = TRUE), ]
head(table4)
``` 

### Creating the input dataset

As mentioned previously, n-grams are a core component of our model.  Our goal is to 
build a text prediction model, and we will begin by creating a simple model based on
n-grams. Specifically, our model will rely on a dataset with three columns containing:

1. the first n-1 words in the n-gram
2. a prediction which is the n/superscript^th word of the n-gram
3. a count of how many times the n-gram appears in the merged corpus

This is done using the below code. This script takes about 36 hours to run on an Intel i5
processor, 64 GB RAM Dell laptop.

```{r include = TRUE, eval = FALSE}
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
two_temp <- two_temp[1:500000, ]

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
two_grams_tidy <- two_grams_tidy[!duplicated(two_grams_tidy[, 1]), ]
save(two_grams_tidy, file = "two_grams_tidy_500000.RData")

# Make 3-grams frequency table
three_temp <- tokens_ngrams(training_us_tokens, n = 3L, skip = 0L, concatenator = "_")
three_temp <- as.data.table(table(unlist(three_temp)))
three_temp <- three_temp[order(N, decreasing = TRUE)]
save(three_temp, file = "three_temp.RData")
three_temp <- three_temp[1:400000, ]

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
three_grams_tidy <- three_grams_tidy[!duplicated(three_grams_tidy[, 1]), ]
save(three_grams_tidy, file = "three_grams_tidy_400000.RData")

# Make 4-grams frequency table
four_temp <- tokens_ngrams(training_us_tokens, n = 4L, skip = 0L, concatenator = "_")
four_temp <- as.data.table(table(unlist(four_temp)))
four_temp <- four_temp[order(N, decreasing = TRUE)]
save(four_temp, file = "four_temp.RData")
four_temp <- four_temp[1:300000, ]

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
four_grams_tidy <- four_grams_tidy[!duplicated(four_grams_tidy[, 1]), ]
save(four_grams_tidy, file = "four_grams_tidy_300000.RData")

# Make 5-grams frequency table
five_temp <- tokens_ngrams(training_us_tokens, n = 5L, skip = 0L, concatenator = "_")
five_temp <- as.data.table(table(unlist(five_temp)))
head(five_temp)
five_temp <- five_temp[order(N, decreasing = TRUE)]
save(five_temp, file = "five_temp.RData")
five_temp <- five_temp[1:300000, ]

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
five_grams_tidy <- five_grams_tidy[!duplicated(five_grams_tidy[, 1]), ]
save(five_grams_tidy, file = "five_grams_tidy_300000.RData")


# Make 6-grams frequency table
six_temp <- tokens_ngrams(training_us_tokens, n = 6L, skip = 0L, concatenator = "_")
six_temp <- as.data.table(table(unlist(six_temp)))
six_temp <- six_temp[order(N, decreasing = TRUE)]
save(six_temp, file = "six_temp.RData")
six_temp <- six_temp[1:300000, ]

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
six_grams_tidy <- six_grams_tidy[!duplicated(six_grams_tidy[, 1]), ]
save(six_grams_tidy, file = "six_grams_tidy_300000.RData")

# Model Input dataset
input_training <- rbindlist(l = list(two_grams_tidy,
                                three_grams_tidy,
                                four_grams_tidy,
                                five_grams_tidy,
                                six_grams_tidy),
                                idcol = TRUE)
View(input_training)
```

This code will create the dataset that will be used to create our first text 
prediction model, a simple model based on n-grams.  

### Testing Set

In a very simlar manner, we build a testing set to evaluate our model's accuracy.  

We tested model accuracy by partitioning the original corpus into training and
testing corpora. We then sampled 250,000 random n-grams from the testing corpus 
and and evaluated our model over this set.

Our model shows accuracy of 5.87% (see code snippet below). In other words, given
a random input of text (an n-gram), our model is correct approximately 5.87% of the time.

Note there are approximately 9,000 unique words required to span about 90%  of the 
written English language. This means that a "random guess" of the next word would be 
correct less than 0.01% of the time.

This means our model's 5.87% accuracy is approximately 600x better than a random guess.


```{r include = TRUE, eval = FALSE}
# Make 2-grams frequency table
two_temp_testing <- tokens_ngrams(testing_us_tokens, n = 2L, skip = 0L, concatenator = "_")
two_temp_testing <- as.data.table(unlist(two_temp_testing))
two_temp_testing <- two_temp_testing[sample(nrow(two_temp_testing), 50000)]
save(two_temp_testing, file = "two_temp_testing.RData")

x <- NULL
y <- NULL
z <- NULL
two_grams_tidy_testing <- NULL

for (i in 1:nrow(two_temp_testing[, 1])) {
        z <- list(strsplit(as.character(two_temp_testing[i, 1]), split = "_")[[1]][1])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(two_temp_testing[, 1])) {
        z <- unlist(strsplit(as.character(two_temp_testing[i, 1]), split = "_"))[2]
        y <- rbind(y, z)
}

two_grams_tidy_testing <- data.table(x, y)
names(two_grams_tidy_testing) <- c("n-1", "n")
save(two_grams_tidy_testing, file = "two_grams_tidy_testing_50000.RData")

# Make 3-grams frequency table
three_temp_testing <- tokens_ngrams(testing_us_tokens, n = 3L, skip = 0L, concatenator = "_")
three_temp_testing <- as.data.table(unlist(three_temp_testing))
three_temp_testing <- three_temp_testing[sample(nrow(three_temp_testing), 50000)]
save(three_temp_testing, file = "three_temp_testing.RData")

x <- NULL
y <- NULL
z <- NULL
three_grams_tidy_testing <- NULL

for (i in 1:nrow(three_temp_testing[, 1])) {
        z <- list(strsplit(as.character(three_temp_testing[i, 1]), split = "_")[[1]][1:2])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(three_temp_testing[, 1])) {
        z <- unlist(strsplit(as.character(three_temp_testing[i, 1]), split = "_"))[3]
        y <- rbind(y, z)
}

three_grams_tidy_testing <- data.table(x, y)
names(three_grams_tidy_testing) <- c("n-1", "n")
save(three_grams_tidy_testing, file = "three_grams_tidy_testing_50000.RData")

# Make 4-grams frequency table
four_temp_testing <- tokens_ngrams(testing_us_tokens, n = 4L, skip = 0L, concatenator = "_")
four_temp_testing <- as.data.table(unlist(four_temp_testing))
four_temp_testing <- four_temp_testing[sample(nrow(four_temp_testing), 50000)]
save(four_temp_testing, file = "four_temp_testing.RData")

x <- NULL
y <- NULL
z <- NULL
four_grams_tidy_testing <- NULL

for (i in 1:nrow(four_temp_testing[, 1])) {
        z <- list(strsplit(as.character(four_temp_testing[i, 1]), split = "_")[[1]][1:3])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(four_temp_testing[, 1])) {
        z <- unlist(strsplit(as.character(four_temp_testing[i, 1]), split = "_"))[4]
        y <- rbind(y, z)
}

four_grams_tidy_testing <- data.table(x, y)
names(four_grams_tidy_testing) <- c("n-1", "n")
save(four_grams_tidy_testing, file = "four_grams_tidy_testing_50000.RData")

# Make 5-grams frequency table
five_temp_testing <- tokens_ngrams(testing_us_tokens, n = 5L, skip = 0L, concatenator = "_")
five_temp_testing <- as.data.table(unlist(five_temp_testing))
five_temp_testing <- five_temp_testing[sample(nrow(five_temp_testing), 50000)]
save(five_temp_testing, file = "five_temp_testing.RData")

x <- NULL
y <- NULL
z <- NULL
five_grams_tidy_testing <- NULL

for (i in 1:nrow(five_temp_testing[, 1])) {
        z <- list(strsplit(as.character(five_temp_testing[i, 1]), split = "_")[[1]][1:4])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(five_temp_testing[, 1])) {
        z <- unlist(strsplit(as.character(five_temp_testing[i, 1]), split = "_"))[5]
        y <- rbind(y, z)
}

five_grams_tidy_testing <- data.table(x, y)
names(five_grams_tidy_testing) <- c("n-1", "n")
save(five_grams_tidy_testing, file = "five_grams_tidy_testing_50000.RData")

# Make 6-grams frequency table
six_temp_testing <- tokens_ngrams(testing_us_tokens, n = 6L, skip = 0L, concatenator = "_")
six_temp_testing <- as.data.table(unlist(six_temp_testing))
six_temp_testing <- six_temp_testing[sample(nrow(six_temp_testing), 50000)]
save(six_temp_testing, file = "six_temp_testing.RData")

x <- NULL
y <- NULL
z <- NULL
six_grams_tidy_testing <- NULL

for (i in 1:nrow(six_temp_testing[, 1])) {
        z <- list(strsplit(as.character(six_temp_testing[i, 1]), split = "_")[[1]][1:5])
        z <- strsplit(stri_join_list(z, sep = "_", collapse = TRUE), " ")[[1]]
        x <- rbind(x, z)
}

for (i in 1:nrow(six_temp_testing[, 1])) {
        z <- unlist(strsplit(as.character(six_temp_testing[i, 1]), split = "_"))[6]
        y <- rbind(y, z)
}

six_grams_tidy_testing <- data.table(x, y)
names(six_grams_tidy_testing) <- c("n-1", "n")
save(six_grams_tidy_testing, file = "six_grams_tidy_testing_50000.RData")

# Model Input dataset
input_testing <- rbindlist(l = list(two_grams_tidy_testing,
                                    three_grams_tidy_testing,
                                    four_grams_tidy_testing,
                                    five_grams_tidy_testing,
                                    six_grams_tidy_testing),
                                    idcol = TRUE)

```

### n-gram Backoff Model

Given a particular n-gram entered by the user, our algorithm first tries to find the 
n-gram in our input dataset, and if it is found, the user receives back the subsequent word 
that most-frequently follows that particular n-gram.

If the user's n-gram is not found, then a list of so-called "skip-grams" is created from the 
n-gram, and the algorithm searches sequentially for each of these skip-grams in the dataset. As 
soon as a match is found, the associated predicted word is returned, just as described above.

In our above example of "I really love football", skip-grams would include "I love football",
"I really football", "really love football" and "I really love".

Our algorithm eliminates and skip-grams which do not contain the final word entered by
the user. In the above example, the n-gram "I really love" would thus not be considered as a 
possible match in our dataset. This improves model accuracy because the final word in the 
input is often very closely related to the prediction word; eliminating it hurts accuracy.

```{r, echo=TRUE, eval = FALSE}
a <- NULL
f <- function(a = NULL) {
        # User provides an n-gram "a"
        toks <- tokens(a)
        toks_list <- tokens_skipgrams(toks,
                                      n = 1:length(toks[[1]]),
                                      skip = 0:4,
                                      concatenator = "_")
        # Re-order token list so it begins with the n-gram (no skips) and proceeds sequentially
        toks_list <- rev(toks_list[[1]])
        # Require the last word entered by user to be matched as part of skip-gram
        last <- toks[[1]][length(toks[[1]])]
        toks_list <- toks_list[grep(last, toks_list)]
        # Loop through the tokens list until a match is found in input dataset
        for (i in 1:length(toks_list)) {
                if (nrow(input[`n-1` == toks_list[i], ]) != 0) {
                        # Return predicted word
                        return(input[`n-1` == toks_list[i], n])
                        break()
                }
        }
}
```

### Accuracy

Next we evaluate accuracy.

```{r include = TRUE, eval = TRUE}
# Merge testing and training sets
merged_dt <- merge(input_testing, input_training, by = "n-1", all.x = TRUE)

# Model accuracy is approximately 5.9%
nrow(merged_dt[n.y == n.x]) / nrow(merged_dt)
```


```{r include = TRUE, eval = TRUE}
# 171,476 words in Second Edition of Oxford English Dictionary
# so our model is 10,000 times better than a random guess
(nrow(merged_dt[n.y == n.x]) / nrow(merged_dt)) / (1/171476)
```
