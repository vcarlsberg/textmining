library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(SentimentAnalysis)
library(syuzhet)
library(data.table)
library(textstem)

twitter_user="@Lambe_Turah"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_data = userTimeline(twitter_user,n=1000)

df <- do.call("rbind", lapply(tweet_data, as.data.frame))
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
a<-read.delim("id.stopwords.02.01.2016.txt")
corpus <- tm_map(corpus, removeWords, a$STOPWORDS)

DTM <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,stopwords = TRUE))
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
#head <- sums[1:75,]
wordcloud(words = sums$term, freq = sums$count, min.freq = 10, 
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

neg=scan("negative.txt",character(),quote="")
pos=scan("positive.txt",character(),quote="")

d <- SentimentDictionaryBinary(neg,pos)
summary(d)


sent <- analyzeSentiment(DTM, language=d)
# were going to just select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# Now lets take a look at what these sentiment values look like. 
#head(sent)
summary(sent$SentimentGI)

sent$sentimentGIx<-cut(sent$SentimentGI,breaks=c(-1,-0.000001,0.000001,1),
                       labels=c("Negative","Neutral","Positive"))

ggplot(data=sent,aes(sentimentGIx))+
  geom_bar(position="stack")+
  scale_fill_grey()

