library("RColorBrewer")
library("quanteda")

ponctuation <- c(".", "'", "!", "^", "?", ",", ":", "#", "-",  "...", "<", ">", "<<<", '"')
#taken from https://github.com/shutterstock/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words
badWords <- VectorSource(readLines("./dirty/english.txt"))

#corp <- GetCorpus("./data/final/en_US/samples/*")
GetCorpus <- function(file){
  corp <- corpus(textfile(file=file)) #specifies the exact folder where my text file(s) is for analysis with tm.
  #corp <- tm_map(corp,content_transformer(function(x) iconv(x, to='UTF-8', sub="")),mc.cores=1)
  corp
}

GetDfm <- function(corp, ng){
  resulting_dfm <- dfm(corp, ignoredFeatures = c(stopwords("english"), badWords$content), ngrams=ng)
  resulting_dfm
}

#The files were broken in minor files in order to reduce memory using
ProcessMultipleFiles <- function(path, ng, pattern = "^.*[.]txt$"){
  files <- list.files(path, pattern = pattern)
  mf_dfm <- NULL
  for(file in files){
    corp <- GetCorpus(paste0(path,file))
    dfm_aux <- GetDfm(corp, ng)
    if(is.null(mf_dfm)){
      mf_dfm <- dfm_aux
    }
    else{
      mf_dfm <- cbind(mf_dfm, dfm_aux)
    }
  }
  mf_dfm
}