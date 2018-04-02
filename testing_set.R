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
testing_us_tokens <- tokens(testing_us_corpus, what = "word", 
                            remove_numbers = TRUE, remove_punct = TRUE,
                            remove_symbols = TRUE, remove_separators = TRUE,
                            remove_twitter = TRUE, remove_hyphens = TRUE, 
                            remove_url = TRUE, ngrams = 1L, skip = 0L, concatenator = "_")

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


# We want to evaluate two-six gram accuracy independently so we don't
# use this part:
# Model Input dataset
# input_testing <- rbindlist(l = list(two_grams_tidy_testing,
#                                three_grams_tidy_testing,
#                                four_grams_tidy_testing,
#                                five_grams_tidy_testing,
#                                six_grams_tidy_testing),
#                                idcol = TRUE)



y <- 0

for (k in 1:nrow(six_grams_tidy_testing[1:100, ])) {
        #toks <- tokens(six_grams_tidy_testing[4,]$`n-1`, concatenator = "_")
        toks <- stri_split_fixed(six_grams_tidy_testing[k, ]$`n-1`, pattern = "_")
        toks <- tail(unlist(toks), 8)
        toks_list <- tokens_skipgrams(toks,
                                      n = 1:length(toks),
                                      skip = 0:4,
                                      concatenator = "_")
        toks_list <- rev(toks_list)
        last <- toks[length(toks)]
        toks_list <- toks_list[grep(last, toks_list)]
        
        x <- NULL

        for (i in 1:length(toks_list)) {
                for(j in 1:length(input_training)) {
                        if (nrow(input_training[which(input_training$`n-1` == toks_list[i]), ]) != 0) {
                                if(six_grams_tidy_testing[k, ]$n %in% 
                                        input_training[which(input_training$`n-1` == toks_list[i]), ]$n) {
                                        
                                        x <- c(x, TRUE)  
                                }
                                else {                                        
                                        x <- c(x, FALSE)    
                                }
                                break
                        }
                        break
                }
        }
        if(sum(x) > 0) { y <- y + 1 }
}

y


# test






for (k in 1:nrow(six_grams_tidy_testing[1:30, ])) {
        #toks <- tokens(six_grams_tidy_testing[4,]$`n-1`, concatenator = "_")
        toks <- stri_split_fixed(six_grams_tidy_testing[k, ]$`n-1`, pattern = "_")
        toks <- tail(unlist(toks), 8)
        toks_list <- tokens_skipgrams(toks,
                                      n = 1:length(toks),
                                      skip = 0:4,
                                      concatenator = "_")
        toks_list <- rev(toks_list)
        last <- toks[length(toks)]
        toks_list <- toks_list[grep(last, toks_list)]

        x <- NULL
        y <- vector(length = 30)
        for (i in 1:length(toks_list)) {
                for(j in 1:length(input_training)) {
                        if (nrow(input_training[which(input_training$`n-1` == toks_list[i]), ]) != 0) {
                                if(six_grams_tidy_testing[k, ]$n %in% 
                                    input_training[which(input_training$`n-1` == toks_list[i]), ]$n) {
                                        x <- c(x, TRUE)    
                                }
                                else {                                        
                                        x <- c(x, FALSE)    
                                }
                                #print(input_training[which(input_training$`n-1` == toks_list[i]), ])
                                break
                        }
                }
        }
        print(sum(x))
}


str(six_grams_tidy_testing)

# Convert to df so next step works
input_training <- as.data.frame(input_training)
input_testing <- as.data.frame(input_testing)

# Create new columns which simply concatentates the n-1 and n columns (sep by underscore)
input_training$new_col <- do.call(paste, c(input_training[c("n-1", "n")], sep = "_"))
input_testing$new_col <- do.call(paste, c(input_testing[c("n-1", "n")], sep = "_"))

# Model has 11.3% accuracy
sum(input_testing$new_col %in% input_training$new_col) / 250000


# Model accuracy for version 1.0 (returning one result only)
# Merge testing and training sets
merged_dt <- merge(input_testing, input_training, by = "n-1", all.x = TRUE)

# Model accuracy is approximately 5.9%
nrow(merged_dt[n.y == n.x]) / nrow(merged_dt)

# 171,476 words in Second Edition of Oxford English Dictionary
# so our model is 10,000 times better than a random guess
(nrow(merged_dt[n.y == n.x]) / nrow(merged_dt)) / (1/171476)
100/171476
