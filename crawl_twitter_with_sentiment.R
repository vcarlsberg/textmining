library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(SentimentAnalysis)
library(syuzhet)
library(data.table)
library(textstem)

api_key="oJmTP5bhCgutk4aQNOvZb4RtG"
api_secret="z51jwqM7kirUKBId7GJ07SMaTxeNtZk4xlrfyUpILO2JKXJMas"
access_token="48937719-ciXusxAAYXZCKvKD1e0P58GxIw7D9DiR7JdkZR2lk"
access_token_secret="Rk5IlscyUxQM4DJ9j6jBeIE7mtfuPWNvxLLMV2muoZ9HX"
twitter_user="@Jokowi"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_data = userTimeline(twitter_user,n=10)

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
corpus <- tm_map(corpus, removeWords, stopwords("english"))




lemma_word=lemmatize_words(corpus)
lemma_word

DTM <- DocumentTermMatrix(corpus,
                          control = list(removePunctuation = TRUE,stopwords = TRUE))
inspect(DTM)

sums <- as.data.frame(colSums(as.matrix(DTM)))
sums <- rownames_to_column(sums) 
colnames(sums) <- c("term", "count")
sums <- arrange(sums, desc(count))
#head <- sums[1:75,]
wordcloud(words = sums$term, freq = sums$count, min.freq = 5, 
          max.words=100, random.order=FALSE, rot.per=0.35, 
          colors=brewer.pal(8, "Dark2"))

sent <- analyzeSentiment(DTM, language = "english")
# were going to just select the Harvard-IV dictionary results ..  
sent <- sent[,1:4]
#Organizing it as a dataframe
sent <- as.data.frame(sent)
# Now lets take a look at what these sentiment values look like. 
#head(sent)
summary(sent$SentimentGI)

sent2 <- get_nrc_sentiment(df$text)
# Let's look at the corpus as a whole again:
sent3 <- as.data.frame(colSums(sent2))
sent3 <- rownames_to_column(sent3) 
colnames(sent3) <- c("emotion", "count")

sent3$emotion <- factor(sent3$emotion, levels = sent3$emotion)

ggplot(sent3[1:8,1:2], 
       aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Emotion", y = "Total Count") + 
  ggtitle("Sentiment of tweet") + 
  theme(plot.title = element_text(hjust=0.5))

ggplot(sent3[9:10,1:2], 
       aes(x = emotion, y = count, fill = emotion)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  theme(legend.position="none", panel.grid.major = element_blank()) + 
  labs( x = "Emotion", y = "Total Count") + 
  ggtitle("Positivity of tweet") + 
  theme(plot.title = element_text(hjust=0.5))

