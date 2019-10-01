library(twitteR)
library(tm)
library(tidyverse)
library(wordcloud)
library(SentimentAnalysis)
library(syuzhet)
library(data.table)

twitter_user="@FoxNews"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
tweet_data = userTimeline(twitter_user,n=500)

df <- do.call("rbind", lapply(tweet_data, as.data.frame))
dim(df)

abstract="This study explored the pattern of video 
game usage and video game addiction among male 
college students and examined how video game addiction 
was related to expectations of college engagement, college grade point average (GPA), 
and on-campus drug and alcohol violations. 
Participants were 477 male, first year students at a liberal arts college. 
In the week before the start of classes, participants were given two surveys: 
one of expected college engagement, and the second of video game usage, 
including a measure of video game addiction. 
Results suggested that video game addiction is (a) negatively correlated with expected college 
engagement, (b) negatively correlated with college GPA, even when controlling for high school 
GPA, and (c) negatively correlated with drug and alcohol violations that occurred during the first 
year in college. 
Results are discussed in terms of implications for male students’ engagement and success in college, and in terms of the construct validity of video game addiction."

corpus <- SimpleCorpus(VectorSource(abstract))
# 1. Stripping any extra white space:
corpus <- tm_map(corpus, stripWhitespace)
# 2. Transforming everything to lowercase
corpus <- tm_map(corpus, content_transformer(tolower))
# 3. Removing numbers 
corpus <- tm_map(corpus, removeNumbers)
# 4. Removing punctuation
corpus <- tm_map(corpus, removePunctuation)
# 5. Removing stop words
corpus <- tm_map(corpus, removeWords, c(stopwords("english"),"jakpost","dari","sejak","dengan","lagi",
                                        "dan","juga","selalu","karena",
                                        "ada","yang","kamu","ini","itu","saja","bisa"))

DTM <- DocumentTermMatrix(corpus)
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
head(sent)
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
  labs( x = "Positivity", y = "Total Count") + 
  ggtitle("Sentiment of tweet") + 
  theme(plot.title = element_text(hjust=0.5))

