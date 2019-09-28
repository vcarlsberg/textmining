library(twitteR)
library(tm)
library(SnowballC)
library(wordcloud)
library(tidyverse)
library(SentimentAnalysis)

load("/cloud/project/rdmTweets-201306.RData")
#View(tweets)


df <- do.call("rbind", lapply(tweets, as.data.frame))
dim(df)

corpus <- SimpleCorpus(VectorSource(df$text))
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, stopwords("english"))
#corpus <- tm_map(corpus, stemDocument)

DTM <- DocumentTermMatrix(corpus)
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
#head <- sums[1:75,]
wordcloud(words = sums$term, freq = sums$count, min.freq = 10,
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

#myCorpus <- Corpus(VectorSource(df$text))
#myCorpus <- tm_map(myCorpus, tolower)
#myCorpus <- tm_map(myCorpus, removePunctuation)
#myCorpus <- tm_map(myCorpus, removeNumbers)
#myStopwords <- c(stopwords('english'), "available", "via")
#idx <- which(myStopwords == "r")
#myStopwords <- myStopwords[-idx]
#myCorpus <- tm_map(myCorpus, removeWords, myStopwords)

#dictCorpus <- myCorpus
#myCorpus <- tm_map(myCorpus, stemDocument)


#myCorpus <- tm_map(myCorpus, stemCompletion, dictionary=dictCorpus)

#myDtm <- TermDocumentMatrix(myCorpus)


#inspect(corpus[1:6,1:3])
#findFreqTerms(corpus, lowfreq=10)

#findAssocs(myDtm, 'r', 0.30)

#findAssocs(myDtm, 'miners', 0.30)



#m <- as.matrix(DTM)

#v <- sort(rowSums(m), decreasing=TRUE)
#myNames <- names(v)
#k <- which(names(v)=="miners")
#myNames[k] <- "mining"
#d <- data.frame(word=myNames, freq=v)
#wordcloud(d$word, d$freq, min.freq=50)


sentiment <- analyzeSentiment(DTM, language = "english")
sentiment <- as.data.frame(sentiment)
head(sent)
