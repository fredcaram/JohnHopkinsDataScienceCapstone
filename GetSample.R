library(LaF)
set.seed(1035)
filepath <- "./data/final/en_US/files/en_US.twitter.txt"
twitter_lines <- determine_nlines(filepath)
sample <- sample_lines(filepath, as.integer(twitter_lines * 0.2), nlines = twitter_lines)
sample_size <- length(sample)
training_sample <- sample[1 : (sample_size * 0.8)]
training_sample_size <- (length(training_sample) + 1)  
crossval_sample <- sample[training_sample_size : (training_sample_size + (sample_size * 0.1))]
crossval_sample_size <- length(crossval_sample)
test_sample <- sample[(training_sample_size + crossval_sample_size) : sample_size]
write(training_sample, "./data/final/en_US/samples/en_US.twitter.txt")
write(crossval_sample, "./data/final/en_US/crossval/en_US.twitter.txt")
write(test_sample, "./data/final/en_US/tests/en_US.twitter.txt")

filepath <- "./data/final/en_US/files/en_US.blogs.txt"
blogs_lines <- determine_nlines(filepath)
sample <- sample_lines(filepath, as.integer(twitter_lines * 0.2), nlines = twitter_lines)
sample_size <- length(sample)
training_sample <- sample[1 : (sample_size * 0.8)]
training_sample_size <- (length(training_sample) + 1)  
crossval_sample <- sample[training_sample_size : (training_sample_size + (sample_size * 0.1))]
crossval_sample_size <- length(crossval_sample)
test_sample <- sample[(training_sample_size + crossval_sample_size) : sample_size]
write(training_sample, "./data/final/en_US/samples/en_US.blogs.txt")
write(crossval_sample, "./data/final/en_US/crossval/en_US.blogs.txt")
write(test_sample, "./data/final/en_US/tests/en_US.blogs.txt")

filepath <- "./data/final/en_US/files/en_US.news.txt"
news_lines <- determine_nlines(filepath)
sample <- sample_lines(filepath, as.integer(twitter_lines * 0.2), nlines = twitter_lines)
sample_size <- length(sample)
training_sample <- sample[1 : (sample_size * 0.8)]
training_sample_size <- (length(training_sample) + 1)  
crossval_sample <- sample[training_sample_size : (training_sample_size + (sample_size * 0.1))]
crossval_sample_size <- length(crossval_sample)
test_sample <- sample[(training_sample_size + crossval_sample_size) : sample_size]
write(training_sample, "./data/final/en_US/samples/en_US.news.txt")
write(crossval_sample, "./data/final/en_US/crossval/en_US.news.txt")
write(test_sample, "./data/final/en_US/tests/en_US.news.txt")

rm(twitter_lines)
rm(blogs_lines)
rm(news_lines)
rm(sample)
rm(sample_size)
rm(training_sample)
rm(training_sample_size)
rm(crossval_sample)
rm(crossval_sample_size)
rm(test_sample)


find_in_file <- function(filepath, text){
  results <- grep(text,readLines(filepath), ignore.case=TRUE)
  results
}

find_in_file_result <- function(filepath, text){
  results <- grep(text,readLines(filepath), ignore.case=TRUE, value=TRUE)
  results
}

biggest_lines <- function(filepath){
  file_size <- determine_nlines(filepath)
  cn <- file(filepath, open = "r")
  i <- 1
  l <- 0
  while(i < file_size){
    line <- readLines(cn, 1)
    if(nchar(line) > l){
      l <- nchar(line)
    }
    i <- i + 1
  }
  close(cn)
  l
}