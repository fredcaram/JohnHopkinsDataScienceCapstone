library("data.table")
library("stringr")
source("TokenizeData_tm.R")

#corp <- tm.GetCorpus("./data/final/en_US/samples/")
#corp <- tm.CleanCorpus(corp)
#kgram <- get_kgram(corp)
get_kgram <- function(corp){
  unigram_dtm <- tm.GetDTM(corp, 1, NULL, TRUE)
  unigram_freq <- tm.GetTermsFrequency(unigram_dtm)
  
  bigram_dtm <- tm.GetDTM(corp, 2, 0.6, TRUE)
  bigram_freq <- tm.GetTermsFrequency(bigram_dtm)
  
  trigram_dtm <- tm.GetDTM(corp, 3, 0.6, FALSE)
  trigram_freq <- tm.GetTermsFrequency(trigram_dtm)
  
  tetragram_dtm <- tm.GetDTM(corp, 4, 0.6, FALSE)
  tetragram_freq <- tm.GetTermsFrequency(tetragram_dtm)
  
  kgram = list("1"=unigram_freq, "2"=bigram_freq, "3"=trigram_freq, "4"=tetragram_freq)
  kgram
}

#Based on http://smithamilli.com/blog/kneser-ney/

elements_starting_with <- function(phrase_vector, prewords){
  #grep(pattern = paste0("^", prewords, " .*$"), x=phrase_vector, value=FALSE)
  str_detect(phrase_vector, paste0("^", prewords, " .*$"))
}

elements_ending_with <- function(phrase_vector, ending_words){
  #grep(pattern = paste0("^.* ", ending_words, "$"), x=phrase_vector, value=FALSE)
  str_detect(phrase_vector, paste0("^.* ", ending_words, "$"))
}

get_phrase_length <- function(phrase){
  r <- gregexpr(" ", phrase)
  if(r[[1]][1] == -1){
    word_count <- 1
  }
  else{
    word_count <- length(r[[1]]) + 1
  }
  word_count
}

stupid_backoff <- function(alpha_vector, prewords, kgrams, maxkgram, number_of_recomendations = 3){
  prewords_corp <- tm.GetTextCorpus(prewords)
  prewords_corp <- tm.CleanCorpus(prewords_corp)
  prewords_content <- prewords_corp[[1]]$content
  
  k = 1
  prewords_length <- get_phrase_length(prewords_content)
  if(maxkgram > prewords_length + 1){
    k <- prewords_length + 1
  }
  else{
    k <- maxkgram
  }
  
  results <- NULL
  
  while(k > 1){
    words_aux <- get_n_last_words(prewords_content, k - 1)
    print(k)
    print(words_aux)
    kgram = kgrams[[k]]
    found_elements <- kgram[elements_starting_with(names(kgram), words_aux)]
    found_elements <- found_elements[order(found_elements, decreasing = TRUE)]
    if(length(found_elements) > 0){
      results <- found_elements[1:number_of_recomendations]
      break;
    }
    k <- k - 1
  }
  
  if(is.null(results)){
    elements <- kgrams[[1]]
    elements <- elements[order(elements, decreasing = TRUE)]
    results <- elements[1:number_of_recomendations]
  }
  results
}

simple_interpolation <- function(alpha_vector, prewords, kgrams, maxkgram, number_of_recomendations = 3){
  prewords_corp <- tm.GetTextCorpus(prewords)
  prewords_corp <- tm.CleanCorpus(prewords_corp)
  prewords_content <- prewords_corp[[1]]$content
  
  k = 1
  prewords_length <- get_phrase_length(prewords_content)
  if(maxkgram > prewords_length + 1){
    k <- prewords_length + 1
  }
  else{
    k <- maxkgram
  }
  
  probabilities <- data.table(word="", prob=0, weight=0)
  
  while(k > 1){
    words_aux <- word(prewords_content, -(k - 1), -1)
    kgram = kgrams[[k]]
    found_elements <- kgram[elements_starting_with(names(kgram), words_aux)]
    
    if(length(found_elements) > 0){
      names(found_elements) <- word(names(found_elements), -1)
      total_count <- sum(found_elements)
      p <- data.table(word=names(found_elements), prob=(found_elements / total_count), weight=alpha_vector[k])
      probabilities <- rbindlist(list(probabilities, p))
    }
    k <- k - 1
  }
  
  found_elements <- kgrams[[1]]
  total_count <- sum(found_elements)
  p <- data.table(word=names(found_elements), prob=(found_elements / total_count), weight=alpha_vector[k])
  probabilities <- rbindlist(list(probabilities, p))
  probabilities$weighted_probabilites <- probabilities$prob * probabilities$weight
  results <- probabilities[,sum(weighted_probabilites), by=word]
  setnames(results,"V1","Probabilities")
  setorder(results, -Probabilities, na.last = TRUE)
  
  if(is.null(number_of_recomendations)){
    results
  }
  else{
    results[1:number_of_recomendations,]
  }
}

get_tokenized_corpus <- function(test_corpus_path, number_of_ngrams){
  options(mc.cores=1)
  test_corp <- tm.GetCorpus(test_corpus_path)
  test_corp <- tm.CleanCorpus(test_corp)
  
  tokenized_test_corpus <- RWeka::NGramTokenizer(test_corp, RWeka::Weka_control(min = number_of_ngrams, max = number_of_ngrams))
  tokenized_test_corpus
}

calc_interpolation_perplexity <- function(alpha_vector, kgram, test_corpus_path, number_of_ngrams){
  options(mc.cores=1)
  test_corp <- tm.GetCorpus(test_corpus_path)
  n <- 0
  prob <- 1
  
  for(i in 1:length(test_corp)){
    doc <- test_corp[[i]]
    for(sentence in content(doc)){
      if(any(grep("^na$", sentence, ignore.case = TRUE))){
        next
      }
      number_of_words <- get_phrase_length(sentence)
      tokenized_sentence <- sentence
      if(number_of_words > number_of_ngrams){
        tokenized_sentence <- RWeka::NGramTokenizer(sentence, RWeka::Weka_control(min = number_of_ngrams, max = number_of_ngrams))
      }
      for(token in tokenized_sentence){
        last_word <- word(token, -1)
        prediction <- simple_interpolation(alpha_vector, token, kgram, number_of_ngrams, NULL)
        pred_prob <- prediction[which(word==last_word),]
        if(length(pred_prob) > 0){
          prob <- prob * 1 / pred_prob$Probabilities
        }
        n <- n + 1
      }
    }
  }
  perplexity <- 0
  if(n > 0){
    perplexity <- prob ^ (1/n)
  }
  perplexity
}