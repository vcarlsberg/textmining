library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(SentimentAnalysis)
library(data.table)
library(fpc)
library(mclust)
library(dplyr)

api_key="oJmTP5bhCgutk4aQNOvZb4RtG"
api_secret="z51jwqM7kirUKBId7GJ07SMaTxeNtZk4xlrfyUpILO2JKXJMas"
access_token="48937719-ciXusxAAYXZCKvKD1e0P58GxIw7D9DiR7JdkZR2lk"
access_token_secret="Rk5IlscyUxQM4DJ9j6jBeIE7mtfuPWNvxLLMV2muoZ9HX"
twitter_user="@detikcom"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_data = userTimeline(twitter_user,n=3200)

df<-twListToDF(tweet_data)
dim(df)
df<-filter(df, created >= "2019-10-19 00:00:00" & created <= "2019-10-23 23:59:59")

corpus <- SimpleCorpus(VectorSource(df$text))
# 1. Menghilangkan whitespace
corpus <- tm_map(corpus, stripWhitespace)
# 2. Konversi ke lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Menghilangkan angka
corpus <- tm_map(corpus, removeNumbers)
# 4. Menghilangkan tanda baca
#corpus <- tm_map(corpus, removePunctuation)
removeSymbols <- function(x) gsub("[^[:alpha:][:space:]]*","",x)
corpus <- tm_map(corpus, content_transformer(removeSymbols))

# 5. Removing stop words
a<-read.delim("id.stopwords.02.01.2016.txt")
corpus <- tm_map(corpus, removeWords, a$STOPWORDS)

DTM <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,stopwords = TRUE)
                          )
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
#head <- sums[1:75,]
dev.off()
wordcloud(words = sums$term, freq = sums$count, min.freq = 10, 
          max.words=100, random.order=FALSE, rot.per=0.35,
          colors=brewer.pal(8, "Dark2"))

neg=scan("negative.txt",character(),quote="")
pos=scan("positive.txt",character(),quote="")

d <- SentimentDictionaryBinary(neg,pos)
summary(d)
#print(d)

#ruleSentiment(DTM, d)
sent<-ruleSentiment(DTM,d)
#sent <- analyzeSentiment(DTM, rules=ruleSentiment(DTM, d))
# were going to just select the Harvard-IV dictionary results ..  
#sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# Now lets take a look at what these sentiment values look like. 
#head(sent)
summary(sent)

sent$sentiment<-cut(sent$sent,breaks=c(-1,-0.5,-0.25,-0.000001,0.000001,0.25,0.5,1),
                       labels=c("Strongly Negative",
                                "Negative",
                                "Mildly Negative",
                                "Neutral",
                                "Mildly Positive",
                                "Positive",
                                "Strongly Positive"))
dev.off()
ggplot(data=sent,aes(sentiment))+
  geom_bar(position="stack")+
  scale_fill_grey()+
  labs(title =paste("Distribusi Sentimen Tweet",twitter_user,sep=" "), 
       x = "Kategori Sentimen", y = "Jumlah Tweet") +
  theme(plot.title = element_text(hjust = 0.5))

distance <- dist(DTM, method = "euclidean")
fit <- hclust(distance, method="ward")
plot(fit)
groups <- cutree(fit, k=10)
rect.hclust(fit, k=10, border="red")
