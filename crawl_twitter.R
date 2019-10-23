library(twitteR)
library(tm)

api_key="oJmTP5bhCgutk4aQNOvZb4RtG"
api_secret="z51jwqM7kirUKBId7GJ07SMaTxeNtZk4xlrfyUpILO2JKXJMas"
access_token="48937719-ciXusxAAYXZCKvKD1e0P58GxIw7D9DiR7JdkZR2lk"
access_token_secret="Rk5IlscyUxQM4DJ9j6jBeIE7mtfuPWNvxLLMV2muoZ9HX"

setup_twitter_oauth(api_key, api_secret, access_token, access_token_secret)
#tweet_bude = userTimeline("@BudeSumiyati",n=100)
tweet_data = userTimeline("@jakpost",n=3000)

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

