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
                            remove_numbers = TRUE, #remove_punct = TRUE,
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

# Model Input dataset
input_testing <- rbindlist(l = list(two_grams_tidy_testing,
                                    three_grams_tidy_testing,
                                    four_grams_tidy_testing,
                                    five_grams_tidy_testing,
                                    six_grams_tidy_testing),
                           idcol = TRUE)
View(tail(input_testing, 50))

# It took about 40-55 seconds to run 1,000 rows; we have 250k rows.
pred_dt <- NULL
for (i in 1:nrow(input_testing[1:1000])) {
        pred <- f(input_testing[i, `n-1`])
        pred_dt <- rbind(pred_dt, pred)
        #pred_vec <- c(pred_vec, f(input_testing[i, `n-1`]))
}
pred_dt



a <- tokens("we went to the store so much yesterday but then")
tail(unlist(a), 7)