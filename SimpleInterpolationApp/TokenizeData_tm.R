library("RColorBrewer")
library("tm")
#library("ngramrr")
library("SnowballC")
library("wordcloud")
library(rJava)
.jinit(parameters="-Xmx128g")
library("RWeka")


#badWords <- VectorSource(readLines("./dirty/english.txt"))

tm.GetCorpus <- function(path){
  corp <- VCorpus(DirSource(path))
  corp <- tm.CleanCorpus(corp)
  corp
}

tm.GetTextCorpus <- function(text){
  corp <- VCorpus(VectorSource(text))
  corp
}

tm.RemoveNaFilter <- function(x) any(grep("^na$", x, ignore.case = TRUE, invert = TRUE))


tm.CleanCorpus <- function(corp, removeStopWords = FALSE){
  corp <- tm_map(corp, removePunctuation)
  corp <- tm_map(corp, removeNumbers)
  corp <- tm_map(corp, content_transformer(tolower))
  if(removeStopWords){
    corp <- tm_map(corp, removeWords, stopwords("english"))
  }
  corp <- tm_map(corp, removeWords, badWords$content)
  corp <- tm_map(corp, stripWhitespace)
  corp <- tm_map(corp, PlainTextDocument)  
  corp <- tm_filter(corp, tm.RemoveNaFilter)
  corp
}

tm.GetDTM <- function(corp, ng, sparsity, removeStopWords){
  options(mc.cores=1)
  ptm <- proc.time()
  ngramTokenizer <- function(x) RWeka::NGramTokenizer(x, RWeka::Weka_control(min = ng, max = ng))
  if(removeStopWords){
    corp <- tm_map(corp, removeWords, stopwords("english"))
  }
  my_dtm <- DocumentTermMatrix(corp, control = list(tokenize = ngramTokenizer))
  if(!is.null(sparsity))
  {
    my_dtm <- removeSparseTerms(my_dtm, sparse= sparsity)
  }
  
  # Stop the clock
  print(proc.time() - ptm)
  my_dtm
}

tm.GetTermsFrequency <- function(mydtm){
  freq <- colSums(as.matrix(mydtm))   
  freq <- freq[order(freq * -1)]
  freq
}

tm.PlotFrequency <- function(freq){
  qplot(freq, xlim=c(0, 50), bins=100)
}

tm.PlotWordCloud <- function(freq, n){
  wordcloud(names(head(freq, n=n)), head(freq, n=n), c(4,.01), colors = brewer.pal(6, "Dark2"))
}